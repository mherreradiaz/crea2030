library(terra)
library(tidyverse)
library(tidyterra)
library(patchwork)
library(ClusterR)
library(tsfeatures)

v <- vect('data/processed/vectorial/sitio/cuenca.shp')
r_raw <- rast('data/processed/raster/GW_proxy/WS_SM_acum.tif') |> 
  mask(v)

r <- lapply(r_raw,\(ly) {
  v <- values(ly)
  op <- boxplot.stats(v)$out
  ly[which(v %in%op)] <- NA
  ly
}) |> # filtro de outliers
  rast()

cluster_plot <- \(data,method = 'kmeans',clusters = 4,minPts = 5,data_original,r) {
  require(tidyterra)
  require(patchwork)
  require(dbscan)
  
  if (method == 'hdbscan') {
    data_cluster <- data |> 
      mutate(cluster = factor(1 + hdbscan(select(data,-id), 
                                          minPts = minPts)$cluster,
                              levels = 1:(1+max(hdbscan(select(data,-id), 
                                                        minPts = minPts)$cluster))),
             .before = 2) |> 
      select(id,cluster)
    
    title = 'hdscan'
            
  } else {
    data_cluster <- data |> 
      mutate(cluster = factor(kmeans(select(data,-id), 
                                     centers = clusters)$cluster,
                              levels = 1:clusters),
             .before = 2) |> 
      select(id,cluster)
    
    title = 'kmeans'
  }

  raster <- ifel(!is.na(r),NA,r)[[1]] |> 
    setValues(tibble(id = 1:(nrow(r)*ncol(r))) |> 
                left_join(distinct(select(data_cluster,id,cluster))) |> 
                pull(cluster))
  
  p1 <- ggplot() +
    geom_spatraster(data = raster) +
    scale_fill_brewer(palette = "Spectral", na.translate = FALSE) +
    theme_bw() +
    theme(legend.position = 'none')
  p2 <- data_cluster |> 
    group_by(cluster) |> 
    reframe(n = n()) |> 
    ggplot(aes(cluster,n,fill = cluster)) +
    geom_col() +
    scale_fill_brewer(palette = "Spectral", na.translate = FALSE) +
    labs(fill = "cluster") +
    theme_bw()
  p3 <- data_original |> 
    left_join(data_cluster) |> 
    group_by(id) |> 
    mutate(WS_SM_acum_std = as.numeric(scale(WS_SM_acum))) |> 
    ungroup() |> 
    pivot_longer(cols=contains('WS_SM'), names_to = 'vartype',values_to = 'value') |> 
    mutate(vartype = case_when(vartype == 'WS_SM_acum' ~ 'normal',
                               .default = 'std'),
           año = as.numeric(año)) |> 
    # filter(vartype == 'normal') |> 
    ggplot(aes(año,value,color = cluster)) +
    geom_line(aes(group = id), linewidth = .7, alpha = .01) +
    geom_smooth(aes(group = cluster), method = 'loess',span = .2, 
                linewidth = 1.5, se =F, alpha = .7) +
    scale_color_brewer(palette = "Spectral", na.translate = FALSE) +
    scale_x_continuous(expand = c(0,0.25), breaks = seq(2000,2024,by=4),
                       minor_breaks = 2000:2024) +
    facet_wrap(~vartype,ncol = 1, scales = 'free_y') +
    labs(x = NULL) +
    theme_bw() +
    theme(strip.background = element_rect(fill='white'),
          legend.position = 'none')
  
  layout_custom <- c(
    area(t = 1, b = 3, l = 1, r = 2),  
    area(t = 1, b = 2, l = 3, r = 4),  
    area(t = 3, b = 3, l = 3, r = 4)  
  )
  
  p3 + p1 + p2 + 
    plot_layout(design = layout_custom,
                guides = 'collect') +
    plot_annotation(title = title) &
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

}
  
# dataset

data <- values(r) |> 
  as_tibble() |> 
  mutate(id =1:(nrow(r)*ncol(r)),
         .before = names(r)[1]) |> 
  na.omit() |> 
  pivot_longer(cols = -id, values_to = 'WS_SM_acum',names_to = 'año')

# data |> 
#   ggplot(aes(año,WS_SM_acum)) +
#   geom_line(aes(group = id),alpha = .1,linewidth = 1) +
#   theme_bw()

# ts componentes

data_features <- data |>
  group_by(id) |>
  reframe(
    mean     = mean(WS_SM_acum, na.rm = T),
    median   = median(WS_SM_acum, na.rm = T),
    sd       = sd(WS_SM_acum, na.rm = T),
    min      = min(WS_SM_acum, na.rm = T),
    max      = max(WS_SM_acum, na.rm = T),
    range    = max - min,
    slope    = coef(lm(WS_SM_acum ~ año))[2],
    delta    = last(WS_SM_acum) - first(WS_SM_acum),
    ratio    = (last(WS_SM_acum) - first(WS_SM_acum)) / abs(first(WS_SM_acum) + 1e-6),
    first_diff_sd = sd(diff(WS_SM_acum), na.rm = TRUE),
    n_turns  = sum(diff(sign(diff(WS_SM_acum))) != 0, na.rm = TRUE)
  )

optimal_cluster <- data_features |> 
  select(-id) |> 
  as.matrix() |> 
  Optimal_Clusters_KMeans(
    max_clusters = 10,
    criterion    = "silhouette",
    num_init     = 5,
    initializer  = "kmeans++",
    tol_opt      = 0.002,
    seed         = 123
  ) |> 
  which.max()

cluster_plot(data_features,method = 'kmeans',optimal_cluster,data_original = data,r = r)
cluster_plot(data_features,method = 'kmeans',4,data_original = data,r = r)
cluster_plot(data_features,method = 'hdbscan',minPts = 10,data_original = data,r = r)

# ts features

data_list <- data |>
  pivot_wider(names_from = año,values_from = WS_SM_acum) |> 
  select(-id) |>          
  as.matrix() |>      
  split(seq_len(length(unique(data$id)))) |> 
  setNames(unique(data$id))

data_ts_list <- lapply(data_list, \(x) ts(x, start = 2000, frequency = 1))

data_tsfeatures <- tsfeatures(data_ts_list) |> 
  mutate(id = as.integer(names(data_list)),
         .before = frequency)

data_tsfeatures$mean <- sapply(data_ts_list, mean)
data_tsfeatures$var  <- sapply(data_ts_list, var)

optimal_cluster <- data_tsfeatures |> 
  select(-id) |> 
  as.matrix() |> 
  Optimal_Clusters_KMeans(
    max_clusters = 10,
    criterion    = "silhouette",
    num_init     = 5,
    initializer  = "kmeans++",
    tol_opt      = 0.002,
    seed         = 123
  ) |> 
  which.max()

cluster_plot(data_tsfeatures,method = 'kmeans',optimal_cluster,data_original = data,r = r)
cluster_plot(data_tsfeatures,method = 'kmeans',2,data_original = data,r = r)
cluster_plot(data_tsfeatures,method = 'hdbscan',minPts = 10,data_original = data,r = r)

# ts estandarizada

data_std <- data |> 
  group_by(id) |> 
  mutate(WS_SM_acum = as.numeric(scale(WS_SM_acum))) |> 
  ungroup() |> 
  pivot_wider(names_from = año, values_from = WS_SM_acum)

optimal_cluster <- data_std |> 
  select(-id) |> 
  as.matrix() |> 
  Optimal_Clusters_KMeans(
    max_clusters = 10,
    criterion    = "silhouette",
    num_init     = 5,
    initializer  = "kmeans++",
    tol_opt      = 0.002,
    seed         = 123
  ) |> 
  which.max()

cluster_plot(data_std,method = 'kmeans',optimal_cluster,data_original = data,r = r)
cluster_plot(data_std,method = 'kmeans',4,data_original = data,r = r)
cluster_plot(data_std,method = 'hdbscan',minPts = 10,data_original = data,r = r)

# ts datos brutos

data_raw <- data |>
  pivot_wider(names_from = año, values_from = WS_SM_acum)

optimal_cluster <- data_raw |> 
  select(-id) |> 
  as.matrix() |> 
  Optimal_Clusters_KMeans(
    max_clusters = 10,
    criterion    = "silhouette",
    num_init     = 5,
    initializer  = "kmeans++",
    tol_opt      = 0.002,
    seed         = 123
  ) |> 
  which.max()

cluster_plot(data_raw,method = 'kmeans',optimal_cluster,data_original = data,r = r)
cluster_plot(data_raw,method = 'kmeans',4,data_original = data,r = r)
cluster_plot(data_raw,method = 'hdbscan',minPts = 7,data_original = data,r = r)



