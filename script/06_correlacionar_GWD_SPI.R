library(tidyverse)
library(tidyterra)
library(terra)
library(RColorBrewer)
library(patchwork)

cor_matrix <- \(df, x_cols, y_cols, method) {
  
  combs <- expand.grid(x = x_cols, y = y_cols, stringsAsFactors = FALSE)
  
  map_dfr(1:nrow(combs), function(i) {
    x <- df[[combs$x[i]]]
    y <- df[[combs$y[i]]]
    test <- cor.test(x, y, use = "pairwise.complete.obs", method = method)
    tibble(
      X_metric = combs$x[i],
      Y_metric = combs$y[i],
      r = test$estimate,
      p_value = test$p.value
    )
  }) |> 
    unite("comparison", X_metric, Y_metric, sep = " vs ") |> 
    ungroup()
}
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
    ggsave(filename = output, plot = p, width = width, height = height)
  }
  
  return(p)
}

data_mes <- read_rds('data/processed/rds/GWD_proxy_mes.rds') |>
  filter(between(year(fecha),2000,2021))
data_año <- read_rds('data/processed/rds/GWD_proxy_año.rds')

# correlacion mensual

x_cols = grep('GWD',names(data_mes),value = T)
y_cols = grep('WS|SPI',names(data_mes),value = T)

data_mes |> 
  group_by(codigo) |>
  group_modify(~ cor_matrix(.x, x_cols, y_cols,method = 'pearson')) |> 
  write_rds('data/processed/rds/correlacion_GWD_proxy_mes_pearson.rds')

data_mes |> 
  group_by(codigo) |> 
  group_modify(~ cor_matrix(.x, x_cols, y_cols,method = 'spearman')) |> 
  write_rds('data/processed/rds/correlacion_GWD_proxy_mes_spearman.rds.rds')

# correlacion anual

x_cols = grep('GWD',names(data_año),value = T)
y_cols = grep('WS|SPI',names(data_año),value = T)

data_año |> 
  group_by(codigo) |>
  group_modify(~ cor_matrix(.x, x_cols, y_cols,method = 'pearson')) |> 
  write_rds('data/processed/rds/correlacion_GWD_proxy_año_pearson.rds')

data_año |> 
  group_by(codigo) |> 
  group_modify(~ cor_matrix(.x, x_cols, y_cols,method = 'spearman')) |> 
  write_rds('data/processed/rds/correlacion_GWD_proxy_año_spearman.rds.rds')

# visualizar pearson mensual

data_pearson <- read_rds('data/processed/rds/correlacion_GWD_proxy_mes_pearson.rds')

cor_frequency <- data_pearson |>
  group_by(codigo, comparison) |>
  reframe(abs_r = abs(r)) |>
  group_by(codigo) |>
  slice_max(order_by = abs_r, n = 1) |>
  ungroup() |>
  select(comparison) |>
  count(comparison, name = "frequency") |>
  arrange(frequency)

# data_pearson |> 
#   pull(comparison) |> 
#   unique()

plot_cor(data_pearson,paste0('GWD vs ',c('SPI','WS','WS_acum','WS_SM','WS_SM_acum')),
         output = 'output/fig/correlation/pearson/mes/matrix_mes_pearson_all.png')
plot_cor(data_pearson,paste0('GWD vs ',c('WS','WS_lag3','WS_lag6','WS_lag12')),
         output = 'output/fig/correlation/pearson/mes/matrix_mes_pearson_WS_lag.png')
plot_cor(data_pearson,paste0('GWD vs ',c('WS_SM','WS_SM_lag3','WS_SM_lag6','WS_SM_lag12')),
         output = 'output/fig/correlation/pearson/mes/matrix_mes_pearson_WS_SM_lag.png')
plot_cor(data_pearson,paste0('GWD vs ',c('WS_acum','WS_lag3_acum','WS_lag6_acum','WS_lag12_acum')),
         output = 'output/fig/correlation/pearson/mes/matrix_mes_pearson_WS_acum.png')
plot_cor(data_pearson,paste0('GWD vs ',c('WS_SM_acum','WS_SM_lag3_acum','WS_SM_lag6_acum','WS_SM_lag12_acum')),
         output = 'output/fig/correlation/pearson/mes/matrix_mes_pearson_WS_SM_acum.png')

# visualizar spearman mensual

data_spearman <- read_rds('data/processed/rds/correlacion_GWD_proxy_mes_spearman.rds.rds')

cor_frequency <- data_spearman |>
  group_by(codigo, comparison) |>
  reframe(abs_r = abs(r)) |>
  group_by(codigo) |>
  slice_max(order_by = abs_r, n = 1) |>
  ungroup() |>
  select(comparison) |>
  count(comparison, name = "frequency") |>
  arrange(frequency)

# data_pearson |> 
#   pull(comparison) |> 
#   unique()

plot_cor(data_spearman,paste0('GWD vs ',c('SPI','WS','WS_acum','WS_SM','WS_SM_acum')),
         output = 'output/fig/correlation/spearman/mes/matrix_mes_spearman_all.png')
plot_cor(data_spearman,paste0('GWD vs ',c('WS','WS_lag3','WS_lag6','WS_lag12')),
         output = 'output/fig/correlation/spearman/mes/matrix_mes_spearman_WS_lag.png')
plot_cor(data_spearman,paste0('GWD vs ',c('WS_SM','WS_SM_lag3','WS_SM_lag6','WS_SM_lag12')),
         output = 'output/fig/correlation/spearman/mes/matrix_mes_spearman_WS_SM_lag.png')
plot_cor(data_spearman,paste0('GWD vs ',c('WS_acum','WS_lag3_acum','WS_lag6_acum','WS_lag12_acum')),
         output = 'output/fig/correlation/spearman/mes/matrix_mes_spearman_WS_acum.png')
plot_cor(data_spearman,paste0('GWD vs ',c('WS_SM_acum','WS_SM_lag3_acum','WS_SM_lag6_acum','WS_SM_lag12_acum')),
         output = 'output/fig/correlation/spearman/mes/matrix_mes_spearman_WS_SM_acum.png')

# visualizar pearson año

data_pearson <- read_rds('data/processed/rds/correlacion_GWD_proxy_año_pearson.rds')

cor_frequency <- data_pearson |>
  group_by(codigo, comparison) |>
  reframe(abs_r = abs(r)) |>
  group_by(codigo) |>
  slice_max(order_by = abs_r, n = 1) |>
  ungroup() |>
  select(comparison) |>
  count(comparison, name = "frequency") |>
  arrange(frequency)

data_pearson |>
  pull(comparison) |>
  unique()

plot_cor(data_pearson,paste0('GWD vs ',c('SPI','WS','WS_acum','WS_SM','WS_SM_acum')),
         output = 'output/fig/correlation/pearson/año/matrix_año_pearson_all.png')
plot_cor(data_pearson,paste0('GWD vs ',c('WS','WS_lag3','WS_lag6','WS_lag12')),
         output = 'output/fig/correlation/pearson/año/matrix_año_pearson_WS_lag.png')
plot_cor(data_pearson,paste0('GWD vs ',c('WS_SM','WS_SM_lag3','WS_SM_lag6','WS_SM_lag12')),
         output = 'output/fig/correlation/pearson/año/matrix_año_pearson_WS_SM_lag.png')
plot_cor(data_pearson,paste0('GWD vs ',c('WS_acum','WS_lag3_acum','WS_lag6_acum','WS_lag12_acum')),
         output = 'output/fig/correlation/pearson/año/matrix_año_pearson_WS_acum.png')
plot_cor(data_pearson,paste0('GWD vs ',c('WS_SM_acum','WS_SM_lag3_acum','WS_SM_lag6_acum','WS_SM_lag12_acum')),
         output = 'output/fig/correlation/pearson/año/matrix_año_pearson_WS_SM_acum.png')

# visualizar spearman mensual

data_spearman <- read_rds('data/processed/rds/correlacion_GWD_proxy_año_spearman.rds.rds')

cor_frequency <- data_spearman |>
  group_by(codigo, comparison) |>
  reframe(abs_r = abs(r)) |>
  group_by(codigo) |>
  slice_max(order_by = abs_r, n = 1) |>
  ungroup() |>
  select(comparison) |>
  count(comparison, name = "frequency") |>
  arrange(frequency)

# data_pearson |> 
#   pull(comparison) |> 
#   unique()

plot_cor(data_spearman,paste0('GWD vs ',c('SPI','WS','WS_acum','WS_SM','WS_SM_acum')),
         output = 'output/fig/correlation/spearman/año/matrix_año_spearman_all.png')
plot_cor(data_spearman,paste0('GWD vs ',c('WS','WS_lag3','WS_lag6','WS_lag12')),
         output = 'output/fig/correlation/spearman/año/matrix_año_spearman_WS_lag.png')
plot_cor(data_spearman,paste0('GWD vs ',c('WS_SM','WS_SM_lag3','WS_SM_lag6','WS_SM_lag12')),
         output = 'output/fig/correlation/spearman/año/matrix_año_spearman_WS_SM_lag.png')
plot_cor(data_spearman,paste0('GWD vs ',c('WS_acum','WS_lag3_acum','WS_lag6_acum','WS_lag12_acum')),
         output = 'output/fig/correlation/spearman/año/matrix_año_spearman_WS_acum.png')
plot_cor(data_spearman,paste0('GWD vs ',c('WS_SM_acum','WS_SM_lag3_acum','WS_SM_lag6_acum','WS_SM_lag12_acum')),
         output = 'output/fig/correlation/spearman/año/matrix_año_spearman_WS_SM_acum.png')

# visualizar series mensuales

plot_ts <- \(data_ts,data_cor, output = NULL, width = 13, height = 7) {
  data_cor <- data_cor |> 
    filter(comparison %in% c('GWD vs SPI','GWD_mean vs SPI_mean')) |>
    mutate(cor_group = factor(cut(r,
                                  breaks = c(-Inf, 0, 0.2, 0.4, 0.6, 0.8, 1),
                                  labels = c("r < 0", "0 ≤ r < 0.2", "0.2 ≤ r < 0.4",
                                             "0.4 ≤ r < 0.6", "0.6 ≤ r < 0.8", "0.8 ≤ r ≤ 1"),
                                  right = FALSE, include.lowest = TRUE),
                              levels = c("r < 0", "0 ≤ r < 0.2", "0.2 ≤ r < 0.4",
                                         "0.4 ≤ r < 0.6", "0.6 ≤ r < 0.8", "0.8 ≤ r ≤ 1")))
  
  cols_exist <- intersect(c("GWD", "SPI", "GWD_mean", "SPI_mean"), names(data_ts))
  
  data_plot <- data_ts |>
    pivot_longer(cols=all_of(cols_exist),
                 values_to = 'value',names_to = 'variable') |>
    group_by(codigo,variable) |>
    mutate(value = as.numeric(scale(value))) |> 
    ungroup() |> 
    select(codigo,fecha,variable,value) |> 
    left_join(select(data_cor,codigo,cor_group)) |> 
    suppressMessages()
  
  data_n <- data_plot |> 
    na.omit() |> 
    distinct(codigo,cor_group) |> 
    group_by(cor_group) |> 
    reframe(n = n())
  
  p <- data_plot |> 
    na.omit() |>
    ggplot(aes(fecha,value,color=variable)) +
    geom_line(aes(group = interaction(variable, codigo)), alpha = .3) +
    geom_smooth(method = "loess", span = 0.5, linewidth = 1) +
    geom_text(data = data_n,
              aes(x = as.Date("2000-01-01"), y = Inf, label = paste0("n°wells = ", n)),
              hjust = -0.1, vjust = 1.1, inherit.aes = FALSE) +
    facet_wrap(~cor_group) +
    labs(y = 'saled values',x = NULL) +
    scale_x_date(
      breaks = seq(as.Date("2000-01-01"), as.Date("2024-01-01"), by = "4 years"),
      minor_breaks = seq(as.Date("2000-01-01"), as.Date("2024-01-01"), by = "1 year"),
      date_labels = "%Y",
      expand = c(.01,0)) +
    scale_y_continuous(expand = c(0,0)) +
    coord_cartesian(ylim = c(-5, 5)) +
    theme_bw() +
    theme(strip.background = element_rect(fill = 'white'))
  
  if (!is.null(output)) {
    dir_path <- dirname(output)
    ggsave(filename = output, plot = p, width = width, height = height)
  }
  
  return(p)
}

data_mes <- read_rds('data/processed/rds/GWD_proxy_SPI.rds') |>
  filter(between(year(fecha),2000,2021))
data_pearson <- read_rds('data/processed/rds/GWD_proxy_SPI_mes_correlation_pearson.rds')
data_spearman <- read_rds('data/processed/rds/GWD_proxy_SPI_mes_correlation_spearman.rds')

plot_ts(data_mes,data_pearson,'output/fig/correlation_SPI/ts_mes_pearson.png')
plot_ts(data_mes,data_spearman,'output/fig/correlation_SPI/ts_mes_spearman.png')
  
# visualizar series anuales

data_anual <- read_rds('data/processed/rds/GWD_proxy_SPI_anual.rds') |>
  mutate(fecha = as.Date(paste0(año,'-01-01')))
data_pearson <- read_rds('data/processed/rds/GWD_proxy_SPI_anual_correlation_pearson.rds')
data_spearman <- read_rds('data/processed/rds/GWD_proxy_SPI_anual_correlation_spearman.rds')

plot_ts(data_anual,data_pearson,'output/fig/correlation_SPI/ts_anual_pearson.png')
plot_ts(data_anual,data_spearman,'output/fig/correlation_SPI/ts_anual_spearman.png')
