library(tidyverse)
library(tidyterra)
library(terra)
library(RColorBrewer)
library(patchwork)

cleanInf <- \(x) ifelse(is.infinite(x),NA,x)

data <- read_rds('data/processed/rds/water_storage.rds') |> 
  filter(year(fecha) >= 1999) |> 
  rename(WS=ws,WD=m)

# data |> 
#   filter(codigo %in% unique(data_anual$codigo)[1:6]) |> 
#   ggplot(aes(fecha,WS)) +
#   geom_line(linewidth = 1,alpha = .7) +
#   facet_wrap(~codigo,ncol =3) +
#   theme_bw()

data_anual <- data |>
  mutate(año = year(fecha)) |>
  group_by(codigo,año) |>
  reframe(
    # WS
    WSsum    = sum(WS, na.rm = T),
    WSplus   = sum(WS[WS > 0], na.rm = T),
    WSminus  = sum(WS[WS < 0], na.rm = T),
    WSmin    = min(WS, na.rm = T),
    WSmax    = max(WS, na.rm = T),
    WSrange  = WSmax - WSmin,
    # WD
    WDmean   = mean(WD, na.rm = T),
    WDmin    = cleanInf(min(WD, na.rm = T)),
    WDmax    = cleanInf(max(WD, na.rm = T)),
    WDrange  = WDmax - WDmin,
    WDdelta  = mean(tail(WD, 3), na.rm = TRUE) - mean(head(WD, 3), na.rm = TRUE)
  ) |>
  group_by(codigo) |> 
  mutate(
    # ΔWS
    WSdelta   = WSsum - lag(WSsum),
    # anomalías (SSI)
    SSI_WS    = (WSsum  - mean(WSsum, na.rm = TRUE)) / sd(WSsum,  na.rm = TRUE),
    SSI_WD    = (WDmean - mean(WDmean, na.rm = TRUE)) / sd(WDmean, na.rm = TRUE)
  ) |> 
  filter(between(año,2000,2022)) |> 
  # mutate(across(matches('WS|WD'), \(x) as.numeric(scale(x,center=F)))) |>
  ungroup() |> 
  select(-c(WSmin,WSmax,WDmin,WDmax),-matches('mean|sum'))

write_rds(data_anual,'data/processed/rds/water_storage_anual.rds')
  
# data_anual |>
#   pivot_longer(cols=matches('WS|WD'),values_to = 'value',names_to = 'variable') |>
#   separate(col = variable, into = c('variable','metric'), sep = 2) |>
#   filter(variable == 'WS',
#          codigo %in% unique(data_anual$codigo)[1:9]) |>
#   ggplot(aes(año,value,color = metric)) +
#   geom_hline(yintercept = 0, linetype = 'dashed', alpha = .5) +
#   # geom_point(size = 1) +
#   geom_line(linewidth = 1,alpha = .7) +
#   facet_wrap(~codigo,ncol =3) +
#   labs(y = 'scaled values',x = NULL, title = 'WS') +
#   scale_x_continuous(limits = c(2000,2022), breaks = seq(2000,2022,by=4), expand = c(0,0.5)) +
#   theme_bw()

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

data_cor <- data_anual |>
  group_by(codigo) |>
  mutate(norm = cumsum(rnorm(length(año)))) |> 
  reframe(
    cor_mat = list(
      cor(
        cbind(SSI_WS, WSplus, WSminus, WSrange, WSdelta,norm),
        cbind(SSI_WD, WDrange, WDdelta),
        use = "pairwise.complete.obs"
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

write_rds(data_cor,'data/processed/rds/water_storage_correlation.rds')

data_cor |>  
  group_by(codigo, comparison) |>
  reframe(abs_r = abs(r)) |>
  group_by(codigo) |>
  slice_max(order_by = abs_r, n = 1) |>
  ungroup() |> 
  select(comparison) |> 
  count(comparison, name = "frequency") |>
  arrange(frequency) |> 
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
  filter(comparison == c('WSminus vs SSI_WD')) |>
  mutate(gr = cut(r,breaks = rev(c(Inf, 0, -.2, -.4, -.6,-.8)),
                  labels = 1:5,
                  include.lowest = T, right = F)) |> 
  select(codigo,gr)

group_name <- c('r < -0.6','-0.6 < r < -0.4','-0.4 < r < -0.2','-0.2 < r < 0','r > 0')

data_cor |> 
  filter(comparison %in% c('WSminus vs SSI_WD',
                           'WSrange vs SSI_WD',
                           'norm vs SSI_WD')) |> 
  group_by(comparison) |> 
  mutate(codigo = fct_reorder(as.factor(codigo), r,.desc = T)) |> 
  left_join(data_grupo |>  mutate(codigo = as.factor(codigo))) |> 
  ggplot(aes(comparison, y = codigo, fill = r)) +
  geom_tile() +
  geom_text(aes(label = round(r, 2)), size = 3, color = 'grey20') +
  scale_fill_distiller(palette   = "RdBu",direction = 1,limits= c(-1, 1),name = "r") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  labs(x = NULL,y = "well") +
  facet_grid(rows = vars(gr), space = 'free_y', scales = 'free_y', switch = 'y') +
  theme_bw() +
  theme(strip.background = element_rect(fill = 'white'))

ggsave('output/fig/water_correlation.png',height = 6, width = 8)
  
data_anual |>
  pivot_longer(cols=c(WSminus,SSI_WD,WSrange,WDrange),values_to = 'value',names_to = 'variable') |>
  group_by(codigo,variable) |> 
  mutate(value = as.numeric(scale(value,center=F))) |> 
  filter(codigo %in% filter(data_grupo,gr == i)$codigo) |>
  ggplot(aes(año,value,color = variable)) +
  geom_hline(yintercept = 0, linetype = 'dashed', alpha = .5) +
  # geom_point(size = 1) +
  geom_line(linewidth = 1,alpha = .7) +
  facet_wrap(~codigo,ncol =2) +
  labs(y = 'scaled values',x = NULL, title = group_name[i]) +
  scale_x_continuous(limits = c(2000,2022), breaks = seq(2000,2022,by=4), expand = c(0,0.5)) +
  theme_bw() +
  theme(strip.background = element_rect(fill='white'))

ggsave(glue('output/fig/water_series_grupo{i}.png'),height = 6, width = 10)

# vect('data/processed/vectorial/pozos.shp') |> 
#   left_join(data_grupo) |> 
#   writeVector("data/processed/vectorial/pozos_grupo.shp")
