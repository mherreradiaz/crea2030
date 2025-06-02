library(terra)
library(tidyverse)
library(tidyterra)
library(patchwork)

v <- vect('data/processed/vectorial/sitio/cuenca.shp')
r_raw <- rast('data/processed/raster/GW_proxy/WS_SM_acum.tif') |> 
  mask(v)

r <- lapply(r_raw,\(ly) {
  v <- values(ly)
  op <- boxplot.stats(v)$out
  ly[which(v %in%op)] <- NA
  ly
}) |> 
  rast()

data <- values(r) |> 
  as_tibble() |> 
  mutate(id =1:(nrow(r)*ncol(r)),
         .before = names(r)[1]) |> 
  na.omit() |> 
  pivot_longer(cols = -id, values_to = 'WS_SM_acum',names_to = 'año') |>
  group_by(id) |>
  mutate(WS_SM_acum_std = as.numeric(scale(WS_SM_acum,center=F))) |>
  ungroup() |>
  # select(-WS_SM_acum) |> 
  pivot_wider(names_from = 'año',values_from = c('WS_SM_acum','WS_SM_acum_std'))

set.seed(123)

data_cluster <- data |> 
  mutate(cluster_normal = factor(kmeans(select(data,contains('WS_SM_acum_2')), centers = 4)$cluster,
                          levels = 1:4),
         cluster_std = factor(kmeans(select(data,contains('WS_SM_acum_std')), centers = 4)$cluster,
                                 levels = 1:4),
         .before = paste0('WS_SM_acum_',names(r)[1])) |> 
  pivot_longer(cols = contains('WS_SM_acum'), names_to = 'nombre', values_to = 'WS_SM_acum') |>
  separate(nombre, into = c('variable', 'año'), sep = '_(?=\\d{4})') |> 
  mutate(año = as.numeric(año)) |> 
  select(id,año,variable,WS_SM_acum,cluster_normal,cluster_std)

write_rds(data_cluster,'data/processed/rds/gwproxy_cluster.rds')

r_cluster <- ifel(!is.na(r),NA,r)[[1]]

r_cluster_normal <- r_cluster |> 
  setValues(tibble(id = 1:(nrow(r)*ncol(r))) |> 
              left_join(distinct(select(data_cluster,id,cluster_normal))) |> 
              pull(cluster_normal))
r_cluster_std <- r_cluster |> 
  setValues(tibble(id = 1:(nrow(r)*ncol(r))) |> 
  left_join(distinct(select(data_cluster,id,cluster_std))) |> 
  pull(cluster_std))

plot(r_cluster_normal[[1:6]])
plot(r_cluster_std[[1:6]])

plot(r_cluster)
 
#visualizar

data_cluster <- read_rds('data/processed/rds/gwproxy_cluster.rds') 

data_cluster_normal <- data_cluster |> 
  filter(variable== 'WS_SM_acum') |> 
  select(año,cluster = cluster_normal,WS_SM_acum) |> 

p1 <- data_cluster |> 
  filter(variable== 'WS_SM_acum') |> 
  ggplot(aes(año,WS_SM_acum,color=cluster_normal)) +
  # geom_point(aes(group = id)) +
  geom_jitter(size = .1, width = 0.2, height = 0,alpha = .3,) +
  geom_hline(yintercept = 0, linetype = 'dashed',alpha = .7) +
  geom_smooth(method = 'loess', span = .2,linewidth = 1.5) +
  scale_x_continuous(expand = c(0,0.25), breaks = seq(2000,2024,by=4),
                     minor_breaks = 2000:2024) +
  labs(x = NULL) +
  theme_bw() +
  theme(strip.background = element_rect(fill='white'))

p2 <- data_cluster |> 
  filter(variable== 'WS_SM_acum_std') |> 
  ggplot(aes(año,WS_SM_acum,color=cluster_std)) +
  # geom_point(aes(group = id)) +
  geom_jitter(size = .1, width = 0.2, height = 0,alpha = .3,) +
  geom_hline(yintercept = 0, linetype = 'dashed',alpha = .7) +
  geom_smooth(method = 'loess', span = .2,linewidth = 1.5) +
  scale_x_continuous(expand = c(0,0.25), breaks = seq(2000,2024,by=4),
                     minor_breaks = 2000:2024) +
  labs(x = NULL) +
  theme_bw() +
  theme(strip.background = element_rect(fill='white'))

p1 / p2



  
