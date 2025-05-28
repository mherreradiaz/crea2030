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

# data_anual |>
#   pivot_longer(cols=c('GWD_mean','GWD_lead3_mean','GWD_lead6_mean','WS_acum','WS_SM_acum','WS_deficit'),
#                values_to = 'value',names_to = 'variable') |>
#   # group_by(codigo,variable) |>
#   # mutate(value = as.numeric(scale(value))) |>
#   filter(codigo %in% unique(data_anual$codigo)[1:6]) |>
#   # filter(!(codigo %in% pozos_estacionales)) |>
#   ggplot(aes(año,value,color = variable)) +
#   geom_line(linewidth = 1.1, alpha = .6) +
#   facet_wrap(~codigo,ncol=2) +
#   theme_bw() +
#   theme(strip.background = element_rect(fill = 'white'))

data_cor <- data_anual |>
  group_by(codigo) |>
  reframe(cor_mat = list(cor(as.matrix(across(contains("WS"))),
                             as.matrix(across(contains("GWD"))),
                             use = "pairwise.complete.obs"))) |>
  mutate(cor_long = map(cor_mat, \(mat) {
    as.data.frame(mat) |>
      tibble::rownames_to_column("WS_metric") |>
      pivot_longer(cols      = -WS_metric,
                   names_to  = "WD_metric",
                   values_to = "r")})) |>
  select(codigo, cor_long) |>
  unnest(cor_long) |> 
  unite(col = 'comparison',WS_metric,WD_metric,sep = ' vs ')

write_rds(data_cor,'data/processed/rds/water_balance_correlation.rds')

cor_frequency <- data_cor |>
  group_by(codigo, comparison) |>
  reframe(abs_r = abs(r)) |>
  group_by(codigo) |>
  slice_max(order_by = abs_r, n = 1) |>
  ungroup() |>
  select(comparison) |>
  count(comparison, name = "frequency") |>
  arrange(frequency)

# cor_frequency |> 
#   ggplot(aes(x = reorder(comparison, frequency), y = frequency)) +
#   geom_col() +
#   coord_flip() +
#   scale_y_continuous(expand = c(0,0), breaks = seq(0,14,by=2), minor_breaks = 0:14, limits = c(0,13)) +
#   labs(
#     x = "WS vs WD comparison",
#     y = "frequency") +
#   theme_bw()
# 
# ggsave('output/fig/water_correlation_frequency.png',height = 6, width = 8)


# visualizar

data_cor |> # más frecuentes
  filter(comparison %in% rev(tail(pull(cor_frequency,comparison),4))) |>
  pivot_wider(names_from = comparison,values_from = r) |> 
  mutate(cor_mean = rowMeans(abs(pick(-codigo))),
         codigo = fct_reorder(as.factor(codigo),cor_mean,.desc = T)) |> 
  select(-cor_mean) |> 
  pivot_longer(cols = -codigo, names_to  = 'comparison',values_to = 'r') |> 
  mutate(comparison = factor(comparison,levels = rev(tail(pull(cor_frequency,comparison),4)))) |> 
  ggplot(aes(comparison, y = codigo, fill = r)) +
  geom_tile() +
  geom_text(aes(label = round(r, 2)), size = 3, color = 'grey20') +
  scale_fill_distiller(palette   = "RdBu",direction = 1,limits= c(-1, 1),name = "r") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  labs(x = NULL,y = "well") +
  theme_bw() +
  theme(strip.background = element_rect(fill = 'white'))

ggsave('output/fig/water_correlation.png',height = 6, width = 10)

data_cor |> # WS normal y SM con GWD
  filter(comparison %in% c('WS_acum vs GWD_mean','WS_SM_acum vs GWD_mean')) |>
  pivot_wider(names_from = comparison,values_from = r) |> 
  mutate(cor_mean = rowMeans(abs(pick(-codigo))),
         codigo = fct_reorder(as.factor(codigo),cor_mean,.desc = T)) |> 
  select(-cor_mean) |> 
  pivot_longer(cols = -codigo, names_to  = 'comparison',values_to = 'r') |> 
  mutate(comparison = factor(comparison,levels = unique(comparison))) |> 
  ggplot(aes(comparison, y = codigo, fill = r)) +
  geom_tile() +
  geom_text(aes(label = round(r, 2)), size = 3, color = 'grey20') +
  scale_fill_distiller(palette   = "RdBu",direction = 1,limits= c(-1, 1),name = "r") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  labs(x = NULL,y = "well") +
  theme_bw() +
  theme(strip.background = element_rect(fill = 'white'))

ggsave('output/fig/water_correlation_WS_acum_&_WS_SM_acum.png',height = 6, width = 6)

data_cor |> # WS normal y desfases con GWD
  filter(comparison %in% c('WS_acum vs GWD_mean','WS_acum vs GWD_lead3_mean',
                           'WS_acum vs GWD_lead6_mean','WS_acum vs GWD_lead12_mean')) |>
  pivot_wider(names_from = comparison,values_from = r) |> 
  mutate(cor_mean = rowMeans(abs(pick(-codigo))),
         codigo = fct_reorder(as.factor(codigo),cor_mean,.desc = T)) |> 
  select(-cor_mean) |> 
  pivot_longer(cols = -codigo, names_to  = 'comparison',values_to = 'r') |>
  mutate(comparison = factor(comparison,levels = unique(comparison))) |> 
  ggplot(aes(comparison, y = codigo, fill = r)) +
  geom_tile() +
  geom_text(aes(label = round(r, 2)), size = 3, color = 'grey20') +
  scale_fill_distiller(palette   = "RdBu",direction = 1,limits= c(-1, 1),name = "r") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  labs(x = NULL,y = "well") +
  theme_bw() +
  theme(strip.background = element_rect(fill = 'white'))

ggsave('output/fig/water_correlation_WS_acum.png',height = 6, width = 10)

data_cor |> # WS SM y desfases con GWD
  filter(comparison %in% c('WS_SM_acum vs GWD_mean','WS_SM_acum vs GWD_lead3_mean',
                           'WS_SM_acum vs GWD_lead6_mean','WS_SM_acum vs GWD_lead12_mean')) |>
  pivot_wider(names_from = comparison,values_from = r) |> 
  mutate(cor_mean = rowMeans(abs(pick(-codigo))),
         codigo = fct_reorder(as.factor(codigo),cor_mean,.desc = T)) |> 
  select(-cor_mean) |> 
  pivot_longer(cols = -codigo, names_to  = 'comparison',values_to = 'r') |> 
  mutate(comparison = factor(comparison,levels = unique(comparison))) |> 
  ggplot(aes(comparison, y = codigo, fill = r)) +
  geom_tile() +
  geom_text(aes(label = round(r, 2)), size = 3, color = 'grey20') +
  scale_fill_distiller(palette   = "RdBu",direction = 1,limits= c(-1, 1),name = "r") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  labs(x = NULL,y = "well") +
  theme_bw() +
  theme(strip.background = element_rect(fill = 'white'))

ggsave('output/fig/water_correlation_WS_SM_acum.png',height = 6, width = 10)



grupos_norm <- data_cor |>
  filter(comparison == c('norm vs SSI_WD')) |>
  mutate(r = abs(r)) |> 
  arrange(r) |> 
  pull(codigo)

norm_groups <- list(head(grupos_norm,6),tail(grupos_norm,6))

group_name_norm <- c('low correlation group','high correlation group')
  
data_anual |>
  pivot_longer(cols=c(WSminus,SSI_WD,WSaccum,norm),values_to = 'value',names_to = 'variable') |>
  group_by(codigo,variable) |> 
  mutate(value = as.numeric(scale(value,center=F))) |> 
  filter(codigo %in% norm_groups[[i]]) |>
  ggplot(aes(año,value,color = variable)) +
  geom_hline(yintercept = 0, linetype = 'dashed', alpha = .5) +
  # geom_point(size = 1) +
  geom_line(linewidth = 1,alpha = .7) +
  facet_wrap(~codigo,ncol =2) +
  labs(y = 'scaled values',x = NULL, title = group_name_norm[i]) +
  scale_x_continuous(limits = c(2000,2022), breaks = seq(2000,2022,by=4), expand = c(0,0.5)) +
  theme_bw() +
  theme(strip.background = element_rect(fill='white'))

ggsave(glue('output/fig/water_series_grupo{i}.png'),height = 6, width = 10)

# vect('data/processed/vectorial/pozos.shp') |> 
#   left_join(data_grupo) |> 
#   writeVector("data/processed/vectorial/pozos_grupo.shp")
