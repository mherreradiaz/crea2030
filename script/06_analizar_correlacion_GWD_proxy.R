library(tidyverse)
library(tidyterra)
library(terra)
library(RColorBrewer)
library(patchwork)

cor_matrix <- \(df, x_cols, y_cols, method = 'pearson') {
  combs <- expand.grid(x = x_cols, y = y_cols, stringsAsFactors = FALSE)
  
  map_dfr(seq_len(nrow(combs)), \(i) {
    x_var   <- combs$x[i]
    y_var   <- combs$y[i]
    x_vals  <- df[[x_var]]
    y_vals  <- df[[y_var]]
    n_pairs <- sum(is.finite(x_vals) & is.finite(y_vals))
    
    if (n_pairs >= 2) {
      test   <- cor.test(x_vals, y_vals,
                         use    = 'pairwise.complete.obs',
                         method = method)
      r_val  <- unname(test$estimate)
      p_val  <- test$p.value
    } else {
      r_val  <- NA
      p_val  <- NA
    }
    
    tibble(
      comparison = paste(x_var, y_var, sep = ' vs '),
      r          = r_val,
      p_value    = p_val
    )
  })
}
plot_cor_general <- \(df, title = NULL) {
  require(ggplot2)
  require(dplyr)
  require(forcats)
  
  df |>
    mutate(
      sig = case_when(
        p_value < 0.001 ~ '***',
        p_value < 0.01  ~ '**',
        p_value < 0.05  ~ '*',
        TRUE ~ ''
      ),
      comparison = fct_reorder(comparison, r)
    ) |>
    ggplot(aes(x = comparison, y = r, fill = r)) +
    geom_col() +
    geom_text(aes(label = sig), vjust = 0.5, hjust = -0.3, size = 5) +
    coord_flip() +
    scale_fill_distiller(palette = 'RdBu', direction = 1, limits = c(-1, 1)) +
    labs(
      title = title,
      y = "Pearson's r",
      x = NULL
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      legend.position = 'none'
    ) +
    scale_x_discrete(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0), limits = c(0,1))
}
plot_cor <- function(data, comparisons_vector, output = NULL, title = NULL,width = 10, height = 6) {
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
           label = paste0(round(r,2),case_when(p_value < 0.001 ~ "***",
                                               p_value < 0.01  ~ "**",
                                               p_value < 0.05  ~ "*",
                                               TRUE ~ ""))) |> 
    ggplot(aes(comparison, y = codigo, fill = r)) +
    geom_tile() +
    geom_text(aes(label = label), size = 3, color = 'grey20') +
    scale_fill_distiller(palette = "RdBu", direction = 1, limits = c(-1, 1), name = "r") +
    scale_x_discrete(expand = c(0,0)) +
    scale_y_discrete(expand = c(0,0)) +
    labs(x = NULL, y = "well",title = title) +
    facet_grid(~product, scales = 'free_x', space = 'free_x') +
    theme_bw() +
    theme(strip.background = element_rect(fill = 'white'),
          plot.title = element_text(hjust = 0.5))
  
  if (!is.null(output)) {
    dir_path <- dirname(output)
    ggsave(filename = output, plot = p, width = width, height = height)
  }
  
  return(p)
}
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
  
  cols_exist <- intersect(c("GWD", "SPI", "GWD_mean", "SPI_anual"), names(data_ts))
  
  if (length(intersect('año',names(data_ts))) > 0) data_ts <- mutate(data_ts,fecha = as.Date(paste0(año,'-01-01')))
  
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

data_mes <- read_rds('data/processed/rds/GWD_proxy_mes.rds') |>
  filter(between(year(fecha),2000,2021),
         !is.na(GWD))
data_año <- read_rds('data/processed/rds/GWD_proxy_año.rds') |> 
  filter(!is.na(GWD_mean))

# correlacion general ####

# correlacion mensual

names(data_mes)

corr_grace <- data_mes |> # GRACE
  group_by(codigo) |> 
  mutate(GWD = as.numeric(scale(GWD))) |> 
  ungroup() |> 
  group_modify(\(df, key) cor_matrix(df, 
                                     'GWD', 
                                     c('LWE','LWE_SI'))) |> 
  mutate(product = 'GRACE',
         r = round(r,2)) |> 
  arrange(desc(abs(r)))

corr_tc <- data_mes |> # TerraClimate
  group_by(codigo) |> 
  mutate(GWD = as.numeric(scale(GWD))) |> 
  ungroup() |> 
  group_modify(\(df, key) cor_matrix(df, 
                                     'GWD', 
                                     c('SPI_TC','P_ET_SI','Q_SI','SSI'))) |> 
  mutate(product = 'TerraClimate',
         r = round(r,2)) |> 
  arrange(desc(abs(r)))

corr_era <- data_mes |> # ERA-5
  group_by(codigo) |> 
  mutate(GWD = as.numeric(scale(GWD))) |> 
  ungroup() |> 
  group_modify(\(df, key) cor_matrix(df,
                                     'GWD',
                                     'SPI_ERA')) |> 
  mutate(product = 'ERA-5',
         r = round(r,2)) |> 
  arrange(desc(abs(r)))

corr_mes_general <- bind_rows(corr_grace,corr_tc,corr_era) |> 
  select(product, comparison,r,p_value)
write_rds(corr_mes_general,'data/processed/rds/proxy_corr_mes_general.rds')

data_plot <- corr_mes_general |> 
  mutate(comparison = gsub('GWD vs ','',comparison))

plot_cor_general(data_plot,title = "Pearson's r between GWD and monthly variables")

ggsave('output/fig/correlation_2/summary/matrix_general_mes.png',width = 8, height = 6)

# correlacion anual

names(data_año)

corr_grace <- data_año |> # GRACE
  group_by(codigo) |> 
  mutate(GWD_mean = as.numeric(scale(GWD_mean))) |> 
  ungroup() |> 
  group_modify(\(df, key) cor_matrix(df, 
                                     'GWD_mean', 
                                     c('LWE_mean','LWE_SI'))) |> 
  mutate(product = 'GRACE',
         r = round(r,2)) |> 
  arrange(desc(abs(r)))

corr_tc <- data_año |> # TerraClimate
  group_by(codigo) |> 
  mutate(GWD_mean = as.numeric(scale(GWD_mean))) |> 
  ungroup() |> 
  group_modify(\(df, key) cor_matrix(df, 
                                     'GWD_mean', 
                                     c('P_sum','P_ET_sum','Q_sum','SM_sum','deltaSM_sum',
                                       'SPI_TC','SSI','P_ET_SI','Q_SI','deltaSM_SI'))) |> 
  mutate(product = 'TerraClimate',
         r = round(r,2)) |> 
  arrange(desc(abs(r)))

corr_era <- data_año |> # ERA-5
  group_by(codigo) |> 
  mutate(GWD_mean = as.numeric(scale(GWD_mean))) |> 
  ungroup() |> 
  group_modify(\(df, key) cor_matrix(df, 
                                     'GWD_mean', 
                                     'SPI_ERA')) |> 
  mutate(product = 'ERA-5',
         r = round(r,2)) |> 
  arrange(desc(abs(r)))

corr_año_general <- bind_rows(corr_grace,corr_tc,corr_era) |> 
  select(product, comparison,r,p_value)

write_rds(corr_año_general,'data/processed/rds/proxy_corr_año_general.rds')

data_plot <- corr_año_general |> 
  mutate(comparison = gsub('GWD_mean vs ','',comparison))

plot_cor_general(data_plot,title = "Pearson's r between GWD and annual variables")

ggsave('output/fig/correlation_2/summary/matrix_general_año.png',width = 8, height = 6)

# correlacion por pozo ####

# correlacion mensual

corr_grace <- data_mes |> # GRACE
  group_by(codigo) |> 
  group_modify(\(df, key) cor_matrix(df, 
                                     'GWD', 
                                     c('LWE','LWE_SI'))) |> 
  mutate(product = 'GRACE',
         r = round(r,2))

corr_tc <- data_mes |> # TerraClimate
  group_by(codigo) |> 
  group_modify(\(df, key) cor_matrix(df, 
                                     'GWD', 
                                     c('SPI_TC','P_ET_SI','Q_SI','SSI'))) |> 
  mutate(product = 'TerraClimate',
         r = round(r,2))

corr_era <- data_mes |> # ERA-5
  group_by(codigo) |>
  group_modify(\(df, key) cor_matrix(df, 
                                     'GWD', 
                                     'SPI_ERA')) |> 
  mutate(product = 'ERA-5',
         r = round(r,2))

corr_mes_pozo <- bind_rows(corr_grace,corr_tc,corr_era) |> 
  ungroup() |> 
  select(product, comparison,codigo,r,p_value)
write_rds(corr_mes_pozo,'data/processed/rds/proxy_corr_mes_pozo.rds')

# correlacion anual

corr_grace <- data_año |> # GRACE
  group_by(codigo) |>
  group_modify(\(df, key) cor_matrix(df, 
                                     'GWD_mean', 
                                     c('LWE_mean','LWE_SI'))) |> 
  mutate(product = 'GRACE',
         r = round(r,2))

corr_tc <- data_año |> # TerraClimate
  group_by(codigo) |>
  group_modify(\(df, key) cor_matrix(df, 
                                     'GWD_mean', 
                                     c('P_sum','P_ET_sum','Q_sum','SM_sum','deltaSM_sum',
                                       'SPI_TC','SSI','P_ET_SI','Q_SI','deltaSM_SI'))) |> 
  mutate(product = 'TerraClimate',
         r = round(r,2))

corr_era <- data_año |> # ERA-5
  group_by(codigo) |>
  group_modify(\(df, key) cor_matrix(df, 
                                     'GWD_mean', 
                                     'SPI_ERA')) |> 
  mutate(product = 'ERA-5',
         r = round(r,2))

corr_año_pozo <- bind_rows(corr_grace,corr_tc,corr_era) |> 
  ungroup() |> 
  select(product, comparison,codigo,r,p_value)
write_rds(corr_año_pozo,'data/processed/rds/proxy_corr_año_pozo.rds')

# visualizar pearson mensual

data_mes <- read_rds('data/processed/rds/proxy_corr_mes_pozo.rds') |> 
  mutate(comparison = gsub('GWD vs ','',comparison))

cor_frequency <- data_mes |>
  group_by(product,codigo, comparison) |>
  reframe(abs_r = abs(r)) |>
  group_by(codigo) |>
  slice_max(order_by = abs_r, n = 1) |>
  ungroup() |>
  select(comparison) |>
  count(comparison, name = "frequency") |>
  arrange(frequency)

data_mes |>
  select(product,comparison) |>
  distinct()

plot_cor(data_mes,c('LWE','LWE_SI',
                    'SSI','SPI_TC','P_ET_SI','Q_SI',
                    'SPI_ERA'),
         output = 'output/fig/correlation_2/summary/matrix_pozo_mes.png',
         title = "Pearson's r between GWD and monthly variables")

# visualizar pearson año

data_año <- read_rds('data/processed/rds/proxy_corr_año_pozo.rds') |> 
  mutate(comparison = gsub('GWD_mean vs ','',comparison))

cor_frequency <- data_año |>
  group_by(codigo, comparison) |>
  reframe(abs_r = abs(r)) |>
  group_by(codigo) |>
  slice_max(order_by = abs_r, n = 1) |>
  ungroup() |>
  select(comparison) |>
  count(comparison, name = "frequency") |>
  arrange(frequency)

data_año |>
  select(product,comparison) |>
  distinct()

plot_cor(data_año,c('LWE_SI','LWE_mean',
                    'SSI','SPI_TC','P_ET_SI','Q_SI','SM_sum',
                    'SPI_ERA'),
         output = 'output/fig/correlation_2/summary/matrix_pozo_año.png',
         title = "Pearson's r between GWD and annual variables")

# visualizar spearman año

data_spearman <- read_rds('data/processed/rds/correlacion_GWD_proxy_año_spearman.rds')

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

plot_cor(data_spearman,paste0('GWD_mean vs ',c('SPI_anual','lwe_mean','WS_SM_acum')),
         output = 'output/fig/correlation_2/summary/matrix_año_all_spearman.png',width = 12)

plot_cor(data_spearman,rev(tail(cor_frequency$comparison,4)),
         output = 'output/fig/correlation_2/spearman/año/matrix_año_spearman_4th.png')
plot_cor(data_spearman,paste0('GWD_mean vs ',c('SPI_anual','SPI_mean','WS_sum','WS_acum','WS_SM_sum','WS_SM_acum')),
         output = 'output/fig/correlation_2/spearman/año/matrix_año_spearman_all.png',width = 12)
plot_cor(data_spearman,paste0('GWD_mean vs ',c('WS_sum','WS_lag3_sum','WS_lag6_sum','WS_lag12_sum')),
         output = 'output/fig/correlation_2/spearman/año/matrix_año_spearman_WS_lag.png')
plot_cor(data_spearman,paste0('GWD_mean vs ',c('WS_SM_sum','WS_SM_lag3_sum','WS_SM_lag6_sum','WS_SM_lag12_sum')),
         output = 'output/fig/correlation_2/spearman/año/matrix_año_spearman_WS_SM_lag.png')
plot_cor(data_spearman,paste0('GWD_mean vs ',c('WS_acum','WS_lag3_acum','WS_lag6_acum','WS_lag12_acum')),
         output = 'output/fig/correlation_2/spearman/año/matrix_año_spearman_WS_acum.png')
plot_cor(data_spearman,paste0('GWD_mean vs ',c('WS_SM_acum','WS_SM_lag3_acum','WS_SM_lag6_acum','WS_SM_lag12_acum')),
         output = 'output/fig/correlation_2/spearman/año/matrix_año_spearman_WS_SM_acum.png')

# visualizar series mensuales

data_pearson <- read_rds('data/processed/rds/correlacion_GWD_proxy_mes_pearson.rds')
data_spearman <- read_rds('data/processed/rds/correlacion_GWD_proxy_mes_spearman.rds')

plot_ts(data_mes,data_pearson,'output/fig/correlation_2/pearson/mes/ts_mes_pearson.png')
plot_ts(data_mes,data_spearman,'output/fig/correlation_2/spearman/mes/ts_mes_spearman.png')
  
# visualizar series anuales

data_pearson <- read_rds('data/processed/rds/correlacion_GWD_proxy_año_pearson.rds')
data_spearman <- read_rds('data/processed/rds/correlacion_GWD_proxy_año_spearman.rds')

plot_ts(data_año,data_pearson,'output/fig/correlation_2/pearson/año/ts_año_pearson.png')
plot_ts(data_año,data_spearman,'output/fig/correlation_2/spearman/año/ts_año_spearman.png')
