library(tidyverse)
library(tidyterra)
library(terra)
library(RColorBrewer)
library(patchwork)

cleanInf <- \(x) ifelse(is.infinite(x),NA,x)

# pozos con estacionalidad

read_rds('data/processed/rds/water_balance.rds') |> 
  filter(year(fecha) >= 1999) |>
  group_by(codigo,año = year(fecha)) |> 
  mutate(gw_depth = as.numeric(scale(gw_depth,center=F))) |> 
  group_by(codigo, mes = month(fecha)) |> 
  reframe(GWD = mean(gw_depth,na.rm=T)) |> 
  ggplot(aes(mes,GWD)) +
  geom_point() +
  geom_line(linetype = 'dashed',col = 'red') +
  geom_smooth() +
  scale_x_continuous(breaks = 1:12) +
  facet_wrap(~codigo,ncol=9,scales='free_y') +
  theme_bw()

pozos_estacionales <- c(5423017,5423023,5425006,5426005,5426006,
                        5426008,5426015,5426016,5426020,542622,5428008)

# calcular métricas anuales

data_anual <- read_rds('data/processed/rds/water_balance.rds') |> 
  rename(WS = ws,WS_SM = ws_sm,GWD = gw_depth) |>
  filter(!is.na(WS_SM)) |> 
  group_by(codigo) |> 
  mutate(WS_acum = cumsum(WS),
         WS_SM_acum = cumsum(WS_SM),
         GWD_lead3 = lead(GWD,3),
         GWD_lead6 = lead(GWD,6),
         GWD_lead12 = lead(GWD,12)) |> 
  group_by(codigo,año = year(fecha)) |>
  reframe(
    # WS
    WS_sum     = sum(WS, na.rm = T),
    WS_SM_sum  = sum(WS_SM, na.rm = T),
    WS_acum    = WS_acum[month(fecha)==12],
    WS_SM_acum = WS_SM_acum[month(fecha)==12],
    WS_recarga = sum(WS[WS > 0], na.rm = T),
    WS_deficit = sum(WS[WS < 0], na.rm = T),
    WS_SM_recarga = sum(WS_SM[WS_SM > 0], na.rm = T),
    WS_SM_deficit = sum(WS_SM[WS_SM < 0], na.rm = T),
    # GWD
    GWD_mean   = mean(GWD, na.rm = T),
    GWD_delta  = mean(tail(GWD, 3), na.rm = TRUE) - mean(head(GWD, 3), na.rm = TRUE),
    GWD_lead3_mean   = mean(GWD_lead3, na.rm = T),
    GWD_lead3_delta  = mean(tail(GWD_lead3, 3), na.rm = TRUE) - mean(head(GWD_lead3, 3), na.rm = TRUE),
    GWD_lead6_mean   = mean(GWD_lead6, na.rm = T),
    GWD_lead6_delta  = mean(tail(GWD_lead6, 3), na.rm = TRUE) - mean(head(GWD_lead6, 3), na.rm = TRUE),
    GWD_lead12_mean   = mean(GWD_lead12, na.rm = T),
    GWD_lead12_delta  = mean(tail(GWD_lead12, 3), na.rm = TRUE) - mean(head(GWD_lead12, 3), na.rm = TRUE)
  ) |>
  filter(between(año,2000,2021))

write_rds(data_anual,'data/processed/rds/water_balance_anual.rds')

# correlacion

data_anual <- read_rds('data/processed/rds/water_balance_anual.rds')

ws_cols <- names(data_anual)[grepl("^WS", names(data_anual))]
gwd_cols <- names(data_anual)[grepl("^GWD", names(data_anual))]
combs <- expand.grid(ws = ws_cols, gwd = gwd_cols, stringsAsFactors = FALSE)

data_pearson <- data_anual |> # pearson
  group_by(codigo) |>
  group_modify(~{
    df <- .x
    map_dfr(1:nrow(combs), function(i) {
      x <- df[[combs$ws[i]]]
      y <- df[[combs$gwd[i]]]
      test <- cor.test(x, y, use = "pairwise.complete.obs")
      tibble(
        WS_metric = combs$ws[i],
        WD_metric = combs$gwd[i],
        r = test$estimate,
        p_value = test$p.value
      )
    })
  }) |>
  unite("comparison", WS_metric, WD_metric, sep = " vs ") |> 
  ungroup()

data_spearman <- data_anual |> # spearman
  group_by(codigo) |>
  group_modify(~{
    df <- .x
    map_dfr(1:nrow(combs), function(i) {
      x <- df[[combs$ws[i]]]
      y <- df[[combs$gwd[i]]]
      test <- cor.test(x, y, use = "pairwise.complete.obs", method = 'spearman')
      tibble(
        WS_metric = combs$ws[i],
        WD_metric = combs$gwd[i],
        r = test$estimate,
        p_value = test$p.value
      )
    })
  }) |>
  unite("comparison", WS_metric, WD_metric, sep = " vs ") |> 
  ungroup()

write_rds(data_pearson,'data/processed/rds/water_balance_correlation_pearson.rds')
write_rds(data_spearman,'data/processed/rds/water_balance_correlation_spearman.rds')

# visualizar pearson

data_pearson <- read_rds('data/processed/rds/water_balance_correlation_pearson.rds')

plot_cor <- function(data, comparisons_vector, output = NULL, width = 10, height = 6) {
  codigo_order <- data |> 
    filter(comparison %in% comparisons_vector) |>
    group_by(codigo) |> 
    reframe(cor_mean = mean(abs(r))) |> 
    arrange(desc(cor_mean)) |> 
    pull(codigo)
  
  p <- data |> 
    filter(comparison %in% comparisons_vector) |>
    mutate(codigo    = factor(codigo, levels = codigo_order),
           comparison = factor(comparison, levels = comparisons_vector),
           label = paste0(round(r,2),ifelse(p_value < .05,'**',''))) |> 
    ggplot(aes(comparison, y = codigo, fill = r)) +
    geom_tile() +
    geom_text(aes(label = label), size = 3, color = 'grey20') +
    scale_fill_distiller(palette = "RdBu", direction = 1, limits = c(-1, 1), name = "r") +
    scale_x_discrete(expand = c(0,0)) +
    scale_y_discrete(expand = c(0,0)) +
    labs(x = NULL, y = "well") +
    theme_bw() +
    theme(strip.background = element_rect(fill = 'white'))
  
  if (!is.null(output)) {
    dir_path <- dirname(output)
    if (!dir.exists(dir_path)) {
      stop("La carpeta especificada en 'output' no existe: ", dir_path)
    }
    ggsave(filename = output, plot = p, width = width, height = height)
  }
  
  return(p)
}

cor_frequency <- data_pearson |>
  group_by(codigo, comparison) |>
  reframe(abs_r = abs(r)) |>
  group_by(codigo) |>
  slice_max(order_by = abs_r, n = 1) |>
  ungroup() |>
  select(comparison) |>
  count(comparison, name = "frequency") |>
  arrange(frequency)

plot_cor(data_pearson,rev(tail(pull(cor_frequency,comparison),4)),
                 output = 'output/fig/correlation/pearson_4th.png')
plot_cor(data_pearson,c('WS_acum vs GWD_mean','WS_SM_acum vs GWD_mean'),
                 output = 'output/fig/correlation/pearson_both.png',height = 6, width = 6)
plot_cor(data_pearson,c('WS_acum vs GWD_mean','WS_acum vs GWD_lead3_mean',
                                'WS_acum vs GWD_lead6_mean','WS_acum vs GWD_lead12_mean'),
                 output = 'output/fig/correlation/pearson_WS_acum.png')
plot_cor(data_pearson,c('WS_SM_acum vs GWD_mean','WS_SM_acum vs GWD_lead3_mean',
                                'WS_SM_acum vs GWD_lead6_mean','WS_SM_acum vs GWD_lead12_mean'),
                 output = 'output/fig/correlation/pearson_WS_SM_acum.png')

# visualizar spearman

cor_frequency <- data_spearman |>
  group_by(codigo, comparison) |>
  reframe(abs_r = abs(r)) |>
  group_by(codigo) |>
  slice_max(order_by = abs_r, n = 1) |>
  ungroup() |>
  select(comparison) |>
  count(comparison, name = "frequency") |>
  arrange(frequency)

plot_cor(data_spearman,rev(tail(pull(cor_frequency,comparison),4)),
         output = 'output/fig/correlation/spearman_4th.png')
plot_cor(data_spearman,c('WS_acum vs GWD_mean','WS_SM_acum vs GWD_mean'),
         output = 'output/fig/correlation/spearman_both.png',height = 6, width = 6)
plot_cor(data_spearman,c('WS_acum vs GWD_mean','WS_acum vs GWD_lead3_mean',
                        'WS_acum vs GWD_lead6_mean','WS_acum vs GWD_lead12_mean'),
         output = 'output/fig/correlation/spearman_WS_acum.png')
plot_cor(data_spearman,c('WS_SM_acum vs GWD_mean','WS_SM_acum vs GWD_lead3_mean',
                        'WS_SM_acum vs GWD_lead6_mean','WS_SM_acum vs GWD_lead12_mean'),
         output = 'output/fig/correlation/spearman_WS_SM_acum.png')

# visualizar series

data_anual <- read_rds('data/processed/rds/water_balance_anual.rds')

plot_ts <- \(data,method, output = NULL, width = 12, height = 6) {
  data_cor <- read_rds(paste0('data/processed/rds/water_balance_correlation_',method,'.rds')) |> 
    filter(comparison == 'WS_SM_acum vs GWD_mean') |>
    mutate(cor_group = factor(cut(r,
                                  breaks = c(-1, -0.8, -0.6, -0.4, -0.2, 0, Inf),
                                  labels = c("-0.8 ≥ r ≥ -1", "-0.6 > r ≥ -0.8", "-0.4 > r ≥ -0.6",
                                             "-0.2 > r ≥ -0.4", "0 > r ≥ -0.2", "r > 0"),
                                  right = TRUE, include.lowest = TRUE),
                              levels = c("-0.8 ≥ r ≥ -1", "-0.6 > r ≥ -0.8", "-0.4 > r ≥ -0.6",
                                         "-0.2 > r ≥ -0.4", "0 > r ≥ -0.2", "r > 0")))
  
  data_plot <- data_anual |>
    pivot_longer(cols=c('GWD_mean','WS_SM_acum'),
                 values_to = 'value',names_to = 'variable') |>
    group_by(codigo,variable) |>
    mutate(value = as.numeric(scale(value))) |> 
    ungroup() |> 
    select(codigo,año,variable,value) |> 
    left_join(select(data_cor,codigo,cor_group)) |> 
    suppressMessages()
  
  grupos <- sort(unique(data_cor$cor_group))
  
  p <- lapply(grupos,\(grupo) {
    data_plot |>
      filter(cor_group == grupo) |> 
      ggplot(aes(año,value,color = variable)) +
      geom_line(linewidth = 1.1, alpha = .6) +
      facet_grid(rows = vars(codigo), cols = vars(cor_group), switch = 'y') +
      labs(y = NULL,x = NULL) +
      scale_x_continuous(expand = c(0,0.15), breaks = seq(2000,2024,by=4), minor_breaks = 2000:2024) +
      theme_bw() +
      theme(strip.background = element_rect(fill = 'white'))
  })
  
  p <- (p[[1]] + p[[2]] + p[[3]]) / (p[[4]] + p[[5]] + p[[6]]) + 
    plot_layout(guides = "collect")  & 
    theme(legend.position = "bottom")
  
  if (!is.null(output)) {
    dir_path <- dirname(output)
    if (!dir.exists(dir_path)) {
      stop("La carpeta especificada en 'output' no existe: ", dir_path)
    }
    ggsave(filename = output, plot = p, width = width, height = height)
  }
  
  return(p)
}
plot_ts_2 <- \(data,method, output = NULL, width = 12, height = 6) {
  data_cor <- read_rds(paste0('data/processed/rds/water_balance_correlation_',method,'.rds')) |> 
    filter(comparison == 'WS_SM_acum vs GWD_mean') |>
    mutate(cor_group = factor(cut(r,
                                  breaks = c(-1, -0.8, -0.6, -0.4, -0.2, 0, Inf),
                                  labels = c("-0.8 ≥ r ≥ -1", "-0.6 > r ≥ -0.8", "-0.4 > r ≥ -0.6",
                                             "-0.2 > r ≥ -0.4", "0 > r ≥ -0.2", "r > 0"),
                                  right = TRUE, include.lowest = TRUE),
                              levels = c("-0.8 ≥ r ≥ -1", "-0.6 > r ≥ -0.8", "-0.4 > r ≥ -0.6",
                                         "-0.2 > r ≥ -0.4", "0 > r ≥ -0.2", "r > 0")))
  
  data_plot <- data_anual |>
    pivot_longer(cols=c('GWD_mean','WS_SM_acum'),
                 values_to = 'value',names_to = 'variable') |>
    group_by(codigo,variable) |>
    mutate(value = as.numeric(scale(value))) |> 
    ungroup() |> 
    select(codigo,año,variable,value) |> 
    left_join(select(data_cor,codigo,cor_group)) |> 
    suppressMessages()
  
  p <- data_plot |> 
    mutate(codigo = as.factor(codigo)) |> 
    ggplot(aes(año,value,color = variable)) +
    geom_line(aes(group = interaction(variable, codigo)),alpha = .3) +
    geom_smooth(method = "loess", span = 0.5, linewidth = 1.25) +
    facet_wrap(~cor_group) +
    labs(y = 'scaled values',x = NULL) +
    scale_x_continuous(expand = c(0,0.3), breaks = seq(2000,2024,by=4), minor_breaks = 2000:2024) +
    theme_bw() +
    theme(strip.background = element_rect(fill = 'white'))
  
  if (!is.null(output)) {
    dir_path <- dirname(output)
    if (!dir.exists(dir_path)) {
      stop("La carpeta especificada en 'output' no existe: ", dir_path)
    }
    ggsave(filename = output, plot = p, width = width, height = height)
  }
  
  return(p)
}

plot_ts_2(data_anual,'pearson','output/fig/correlation/pearson_ts.png')
plot_ts_2(data_anual,'spearman','output/fig/correlation/spearman_ts.png')







