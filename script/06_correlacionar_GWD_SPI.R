library(tidyverse)
library(tidyterra)
library(terra)
library(RColorBrewer)
library(patchwork)

cleanInf <- \(x) ifelse(is.infinite(x),NA,x)
cleanNA <- \(x) x[which(!is.na(x))]
consMean <- \(x,consistency = 1) ifelse(length(cleanNA(x))/length(x) >= consistency,mean(x,na.rm=T),NA)

# calcular métricas anuales

data_mes <- read_rds('data/processed/rds/GWD_proxy_SPI.rds') |>
  filter(between(year(fecha),2000,2021))

data_anual <- data_mes |> 
  group_by(codigo, año = year(fecha)) |> 
  reframe(SPI_mean = consMean(SPI,8/12),
          SPI_lag3_mean = consMean(SPI_lag3,8/12),
          SPI_lag6_mean = consMean(SPI_lag6,8/12),
          SPI_lag12_mean = consMean(SPI_lag12,8/12),
          GWD_mean   = consMean(GWD, .5),
          GWD_delta  = mean(tail(GWD, 3), na.rm = TRUE) - mean(head(GWD, 3), na.rm = TRUE))

write_rds(data_anual,'data/processed/rds/GWD_proxy_SPI_anual.rds')

# correlacion mensual

data_mes <- read_rds('data/processed/rds/GWD_proxy_SPI.rds') |>
  filter(between(year(fecha),2000,2021))

gwd_cols <- grep('GWD',names(data_mes),value = T)
spi_cols <- grep('SPI',names(data_mes),value = T)
combs <- expand.grid(gwd = gwd_cols, spi = spi_cols, stringsAsFactors = FALSE)

data_pearson <- data_mes |> # pearson
  group_by(codigo) |>
  group_modify(~{
    df <- .x
    map_dfr(1:nrow(combs), function(i) {
      x <- df[[combs$gwd[i]]]
      y <- df[[combs$spi[i]]]
      test <- cor.test(x, y, use = "pairwise.complete.obs")
      tibble(
        GWD_metric = combs$gwd[i],
        SPI_metric = combs$spi[i],
        r = test$estimate,
        p_value = test$p.value
      )
    })
  }) |>
  unite("comparison", GWD_metric, SPI_metric, sep = " vs ") |> 
  ungroup()

data_spearman <- data_mes |> # spearman
  group_by(codigo) |>
  group_modify(~{
    df <- .x
    map_dfr(1:nrow(combs), function(i) {
      x <- df[[combs$gwd[i]]]
      y <- df[[combs$spi[i]]]
      test <- cor.test(x, y, use = "pairwise.complete.obs", method = 'spearman')
      tibble(
        GWD_metric = combs$gwd[i],
        SPI_metric = combs$spi[i],
        r = test$estimate,
        p_value = test$p.value
      )
    })
  }) |>
  unite("comparison", GWD_metric, SPI_metric, sep = " vs ") |> 
  ungroup()

write_rds(data_pearson,'data/processed/rds/GWD_proxy_SPI_mes_correlation_pearson.rds')
write_rds(data_spearman,'data/processed/rds/GWD_proxy_SPI_mes_correlation_spearman.rds')

# correlacion anual

data_anual <- read_rds('data/processed/rds/GWD_proxy_SPI_anual.rds')

gwd_cols <- grep('GWD',names(data_anual),value = T)
spi_cols <- grep('SPI',names(data_anual),value = T)
combs <- expand.grid(gwd = gwd_cols, spi = spi_cols, stringsAsFactors = FALSE)

data_pearson <- data_anual |> # pearson
  group_by(codigo) |>
  group_modify(~{
    df <- .x
    map_dfr(1:nrow(combs), function(i) {
      x <- df[[combs$gwd[i]]]
      y <- df[[combs$spi[i]]]
      test <- cor.test(x, y, use = "pairwise.complete.obs")
      tibble(
        GWD_metric = combs$gwd[i],
        SPI_metric = combs$spi[i],
        r = test$estimate,
        p_value = test$p.value
      )
    })
  }) |>
  unite("comparison", GWD_metric, SPI_metric, sep = " vs ") |> 
  ungroup()

data_spearman <- data_anual |> # spearman
  group_by(codigo) |>
  group_modify(~{
    df <- .x
    map_dfr(1:nrow(combs), function(i) {
      x <- df[[combs$gwd[i]]]
      y <- df[[combs$spi[i]]]
      test <- cor.test(x, y, use = "pairwise.complete.obs", method = 'spearman')
      tibble(
        GWD_metric = combs$gwd[i],
        SPI_metric = combs$spi[i],
        r = test$estimate,
        p_value = test$p.value
      )
    })
  }) |>
  unite("comparison", GWD_metric, SPI_metric, sep = " vs ") |> 
  ungroup()

write_rds(data_pearson,'data/processed/rds/GWD_proxy_SPI_anual_correlation_pearson.rds')
write_rds(data_spearman,'data/processed/rds/GWD_proxy_SPI_anual_correlation_spearman.rds')

# visualizar pearson mensual

data_pearson <- read_rds('data/processed/rds/GWD_proxy_SPI_mes_correlation_pearson.rds')

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
         output = 'output/fig/correlation_SPI/matrix_mes_pearson.png')

# visualizar spearman mensual

data_spearman <- read_rds('data/processed/rds/GWD_proxy_SPI_mes_correlation_spearman.rds')

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
         output = 'output/fig/correlation_SPI/matrix_mes_spearman.png')

# visualizar pearson anual

data_pearson <- read_rds('data/processed/rds/GWD_proxy_SPI_anual_correlation_pearson.rds')

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
         output = 'output/fig/correlation_SPI/matrix_anual_4th_pearson.png')
plot_cor(data_pearson,c('GWD_mean vs SPI_mean','GWD_mean vs SPI_lag3_mean',
                        'GWD_mean vs SPI_lag6_mean','GWD_mean vs SPI_lag12_mean'),
         output = 'output/fig/correlation_SPI/matrix_anual_mean_pearson.png')
plot_cor(data_pearson,c('GWD_delta vs SPI_mean','GWD_delta vs SPI_lag3_mean',
                        'GWD_delta vs SPI_lag6_mean','GWD_delta vs SPI_lag12_mean'),
         output = 'output/fig/correlation_SPI/matrix_anual_delta_pearson.png')

# visualizar spearman anual

data_spearman <- read_rds('data/processed/rds/GWD_proxy_SPI_anual_correlation_spearman.rds')

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
         output = 'output/fig/correlation_SPI/matrix_anual_4th_spearman.png')
plot_cor(data_spearman,c('GWD_mean vs SPI_mean','GWD_mean vs SPI_lag3_mean',
                        'GWD_mean vs SPI_lag6_mean','GWD_mean vs SPI_lag12_mean'),
         output = 'output/fig/correlation_SPI/matrix_anual_mean_spearman.png')
plot_cor(data_spearman,c('GWD_delta vs SPI_mean','GWD_delta vs SPI_lag3_mean',
                        'GWD_delta vs SPI_lag6_mean','GWD_delta vs SPI_lag12_mean'),
         output = 'output/fig/correlation_SPI/matrix_anual_delta_spearman.png')

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
