library(tidyverse)
library(tidyterra)
library(terra)
library(RColorBrewer)
library(patchwork)

cleanInf <- \(x) ifelse(is.infinite(x),NA,x)

# seleccionar pozos con estacionalidad

read_rds('data/processed/rds/water_balance.rds') |> 
  filter(year(fecha) >= 1999) |> 
  # filter(codigo %in% pozos_estacionales) |> 
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
  # facet_wrap(~codigo,ncol=5) +
  theme_bw()

pozos_estacionales <- c(5423019,5423023,5426005,5426006,5426008,
                        5426015,5426016,5426020,5426022,5428008)

# calcular métricas anuales

data_mes <- read_rds('data/processed/rds/water_balance.rds') |> 
  rename(WS = ws,WS_SM = ws_sm,GWD = gw_depth)

# data |> 
#   filter(codigo %in% unique(data_anual$codigo)[1:6]) |> 
#   ggplot(aes(fecha,WS)) +
#   geom_line(linewidth = 1,alpha = .7) +
#   facet_wrap(~codigo,ncol =3) +
#   theme_bw()

data_anual <- data_mes |>
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
  filter(año >= 2000)
  # group_by(codigo) |> 
  # mutate(
  #   # ΔWS
  #   WSdelta   = WSsum - lag(WSsum),
  #   # anomalías (SSI)
  #   SSI_WS    = (WSsum  - mean(WSsum, na.rm = TRUE)) / sd(WSsum,  na.rm = TRUE),
  #   GWD_SSI    = (WDmean - mean(GWD_mean, na.rm = TRUE)) / sd(GWD_mean, na.rm = TRUE)
  # ) |> 
  # filter(between(año,2000,2022)) |> 
  # mutate(WSaccum   = cumsum(WSsum),
  #        norm = cumsum(rnorm(length(año)))) |> 
  # # mutate(across(matches('WS|WD'), \(x) as.numeric(scale(x,center=F)))) |>
  # ungroup() |> 
  # select(-c(WSmin,WSmax,WDmin,WDmax))

# data |>
#   mutate(año = year(fecha)) |>
#   group_by(codigo,año) |>
#   reframe(WSsum    = sum(WS, na.rm = T)) |>
#   group_by(codigo) |> 
#   mutate(WSaccum_1980   = cumsum(if_else(año >= 1980, WSsum, 0)),
#          WSaccum_1990   = cumsum(if_else(año >= 1990, WSsum, 0)),
#          WSaccum_1999   = cumsum(if_else(año >= 1999, WSsum, 0)),
#          WSaccum_2000   = cumsum(if_else(año >= 2000, WSsum, 0)),
#          WSaccum_2001   = cumsum(if_else(año >= 2001, WSsum, 0)),
#          WSaccum_2005   = cumsum(if_else(año >= 2005, WSsum, 0))) |> 
#   mutate(across(contains('WS'),\(col) if_else(col == 0, NA, col))) |> 
#   ungroup() |>
#   pivot_longer(cols = contains('WSaccum'), names_to = 'var', values_to = 'value') |> 
#   mutate(var = gsub('WSaccum_','',var),
#          var = factor(var,levels = c('2005','2001','2000','1999','1990','1980','WSsum'))) |> 
#   filter(codigo %in% sample(unique(data$codigo),9),
#          año >= 2000) |> 
#   ggplot(aes(año,value,color=var)) +
#   geom_hline(yintercept = 0, linetype = 'dashed', alpha = .5) +
#   geom_line(linewidth = 1, alpha = .7) +
#   facet_wrap(~codigo,ncol=3) +
#   theme_bw()

write_rds(data_anual,'data/processed/rds/water_balance_anual.rds')
  
data_anual |>
  pivot_longer(cols=matches('WS|WD'),values_to = 'value',names_to = 'variable') |>
  separate(col = variable, into = c('variable','metric'), sep = 2) |>
  filter(variable == 'WS',
         codigo %in% unique(data_anual$codigo)[1:9]) |>
  ggplot(aes(año,value,color = metric)) +
  geom_hline(yintercept = 0, linetype = 'dashed', alpha = .5) +
  # geom_point(size = 1) +
  geom_line(linewidth = 1,alpha = .7) +
  facet_wrap(~codigo,ncol =3) +
  labs(y = 'scaled values',x = NULL, title = 'WS') +
  scale_x_continuous(limits = c(2000,2022), breaks = seq(2000,2022,by=4), expand = c(0,0.5)) +
  theme_bw()

# data_cor <- data_anual |>
#   group_by(codigo) |>
#   mutate(norm = cumsum(rnorm(length(año))))

# data_cor |> 
#   pivot_longer(cols = c(SSI_WD,norm),names_to = 'var',values_to = 'value') |> 
#   filter(codigo %in% unique(data_cor$codigo)[1:4]) |> 
#   ggplot(aes(año,value,color = var)) +
#   geom_line() +
#   geom_point() +
#   facet_wrap(~codigo,ncol=2)

set.seed(123)

data_cor <- data_anual |>
  group_by(codigo) |>
  reframe(
    cor_mat = list(
      cor(
        as.matrix(across(contains("WS"))),
        as.matrix(across(contains("GWD"))),
        use = "pairwise.complete.obs",
      )
    )
  ) |>
  mutate(
    cor_long = map(cor_mat, \(mat) {
      as.data.frame(mat) |>
        tibble::rownames_to_column("WS_metric") |>
        pivot_longer(
          cols      = -WS_metric,
          names_to  = "WD_metric",
          values_to = "r"
        )
    })
  ) |>
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

cor_frequency |> 
  ggplot(aes(x = reorder(comparison, frequency), y = frequency)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(expand = c(0,0), breaks = seq(0,14,by=2), minor_breaks = 0:14, limits = c(0,13)) +
  labs(
    x = "WS vs WD comparison",
    y = "frequency") +
  theme_bw()

ggsave('output/fig/water_correlation_frequency.png',height = 6, width = 8)

data_grupo <- data_cor |>
  group_by(codigo) |> 
  reframe(cor_mean = mean(abs(r[comparison %in% tail(cor_frequency$comparison,4)]), na.rm = T)) |> 
  mutate(grupo = ntile(cor_mean, 5))

order_codes <- data_grupo |>
  arrange(desc(cor_mean)) |>
  pull(codigo)

data_cor |> 
  filter(comparison %in% tail(cor_frequency$comparison,4)) |> 
  mutate(codigo = factor(codigo,levels = order_codes)) |> 
  arrange(codigo) |> 
  left_join(data_grupo |>  
              mutate(codigo = factor(codigo,levels = order_codes)) |> 
              select(codigo,grupo)) |> 
  ggplot(aes(comparison, y = codigo, fill = r)) +
  geom_tile() +
  geom_text(aes(label = round(r, 2)), size = 3, color = 'grey20') +
  scale_fill_distiller(palette   = "RdBu",direction = 1,limits= c(-1, 1),name = "r") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  labs(x = NULL,y = "well") +
  facet_grid(rows = vars(grupo), space = 'free_y', scales = 'free_y', switch = 'y') +
  theme_bw() +
  theme(strip.background = element_rect(fill = 'white'))

ggsave('output/fig/water_correlation.png',height = 6, width = 8)

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
