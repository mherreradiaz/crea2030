library(tidyverse)
library(tidyterra)
library(terra)
library(RColorBrewer)
library(patchwork)

cleanInf <- \(x) ifelse(is.infinite(x),NA,x)

data <- read_rds('data/processed/rds/water_storage.rds')

data_anual <- data |> 
  group_by(codigo, año = year(fecha)) |> 
  summarise(WS_anual = sum(ws),
            WS_max = max(ws),
            WS_min = min(ws),
            m_prom = mean(m,na.rm=T),
            m_ini = mean(head(m,3), na.rm=T),
            m_fin = mean(tail(m,3), na.rm=T),
            delta_m = m_fin - m_ini) |> 
  filter(between(año,2000,2022)) |> 
  mutate(WS_acum = cumsum(WS_anual)) |> 
  group_by(codigo) |> 
  mutate(across(c(WS_anual, WS_acum,WS_max, WS_min, m_prom, m_ini, m_fin, delta_m), \(x) as.numeric(scale(x,center=F)))) |> 
  ungroup() |> 
  arrange(codigo,año)

# data_anual |> #variables por pozo
#   filter(codigo %in% well$codigo[i]) |>
#   pivot_longer(cols=c(m_prom,WS_acum),names_to='var',values_to='value') |>
#   ggplot(aes(año,value,color=var)) +
#   geom_point() +
#   geom_line(alpha = .8,linewidth = .8) +
#   geom_hline(yintercept = 0, linetype = 'dashed',alpha = .6) +
#   scale_x_continuous(breaks = seq(2000,2024,by=4),
#                      minor_breaks = seq(2000,2024,by=2),
#                      expand = c(0,.5)) +
#   labs(y = 'scaled value',
#        x = NULL,
#        color = 'variable') +
#   scale_color_discrete(labels = c('mean wd','accum S')) +
#   facet_wrap(~codigo,ncol=2,scale = 'fixed') +
#   guides(color = guide_legend(reverse = TRUE)) +
#   theme_bw()

{p1 <- data_anual |> # WS_anual
    ggplot(aes(año,WS_anual,group=codigo)) +
    geom_hline(yintercept = 0, linetype = 'dashed',alpha = .6) +
    # geom_jitter(width = 0.2, height = 0,alpha = .7,size = 1) +
    geom_line(alpha = .2,linewidth = .8, color = 'dodgerblue3') +
    scale_x_continuous(breaks = seq(2000,2022,by=4),
                       minor_breaks = seq(2000,2022,by=2),
                       expand = c(0,.5)) +
    labs(y = 'anual ws',
         x = NULL) +
    theme_bw()
  
  p2 <- data_anual |> # WS_acum
    ggplot(aes(año,WS_acum,group=codigo)) +
    geom_hline(yintercept = 0, linetype = 'dashed',alpha = .6) +
    # geom_point(color = 'dodgerblue3') +
    geom_line(alpha = .2,linewidth = .8, color = 'dodgerblue3') +
    scale_x_continuous(breaks = seq(2000,2022,by=4),
                       minor_breaks = seq(2000,2022,by=2),
                       expand = c(0,.5)) +
    labs(y = 'anual ws acumulated',
         x = NULL) +
    theme_bw()
  
  p3 <- data_anual |> # m_prom
    ggplot(aes(año,m_prom)) +
    geom_hline(yintercept = 0, linetype = 'dashed',alpha = .6) +
    # geom_point() +
    geom_line(aes(group=codigo),alpha = .2,linewidth = .8, color = 'dodgerblue3') +
    geom_smooth(span=.3, color = 'black') +
    scale_x_continuous(breaks = seq(2000,2022,by=4),
                       minor_breaks = seq(2000,2022,by=2),
                       expand = c(0,.5)) +
    labs(y = 'anual mean well depth',
         x = NULL) +
    theme_bw()
  
  p4 <- data_anual |> # m_delta
    ggplot(aes(año,delta_m)) +
    geom_hline(yintercept = 0, linetype = 'dashed',alpha = .6) +
    # geom_point() +
    geom_line(aes(group=codigo),alpha = .2,linewidth = .8, color = 'dodgerblue3') +
    geom_smooth(span=.3, color = 'black') +
    scale_x_continuous(breaks = seq(2000,2022,by=4),
                       minor_breaks = seq(2000,2022,by=2),
                       expand = c(0,.5)) +
    labs(y = 'anual well depth Δ',
         x = NULL) +
    theme_bw()
  
  (p1 + p2) / (p3 + p4)}

ggsave('output/fig/water.png',height = 7, width = 11)

data_cor <- data_anual |> 
  group_by(codigo) |>
  summarise(WS_anual_cor_m_mean = cor(WS_anual, m_prom, use='complete.obs'),
            WS_acum_cor_m_mean = cor(WS_acum, m_prom, use='complete.obs'),
            WS_max_cor_m_mean = cor(WS_max, m_prom, use='complete.obs'),
            WS_min_cor_m_mean = cor(WS_min, m_prom, use='complete.obs'))

metricas <- data_cor |> 
  pivot_longer(cols = -codigo,names_to  = 'comparison',values_to = 'r') |> 
  mutate(comparison = factor(comparison,levels = names(data_cor)[2:5]))

grupos <- metricas |> 
  filter(comparison == "WS_acum_cor_m_mean") |> 
  mutate(gr = cut(r,breaks = c(-Inf, 0, 0.2, 0.5, 1),
                  labels = 1:4,
                  right = T))

label <- grupos |>
  mutate(x = 2000,
         y = 1,
         label = paste0("r = ", round(r, 2)))

grupos_title <- c('r < 0','0 < r < 0.2','0.2 < r < 0.5','r > 0.5')
  
data_anual |>
  pivot_longer(cols = c(WS_anual,WS_min,m_prom),names_to = 'variable',values_to = 'value') |> 
  select(codigo,año,variable,value) |> 
  left_join(grupos |> select(codigo,gr)) |> 
  filter(gr == sort(unique(grupos$gr))[i]) |> 
  ggplot(aes(año,value,color = variable)) +
  geom_hline(yintercept = 0, linetype = 'dashed', alpha = .5) +
  # geom_point(size = 1) +
  geom_line(linewidth = .5, alpha = .7) +
  facet_wrap(~codigo,ncol =3) +
  labs(x = NULL, title = grupos_title[i]) +
  geom_text(data = label |> filter(gr == sort(unique(grupos$gr))[i]), aes(2002.3, .3, label = label),
    inherit.aes = F) +
  scale_x_continuous(limits = c(2000,2022), breaks = seq(2000,2022,by=4), expand = c(0,0.5)) +
  theme_bw() +
  theme(strip.background = element_rect(fill='white'),
        plot.title = element_text(hjust = 0.5))

ggsave(glue('output/fig/water_series_grupo{i}.png'),height = 8, width = 12)

metricas |> 
  left_join(grupos |> select(codigo,gr)) |> 
  ggplot(aes(comparison, y = as.factor(codigo), fill = r)) +
  geom_tile() +
  geom_text(aes(label = round(r, 2)), size = 3) +
  scale_fill_distiller(palette   = "RdBu",direction = 1,limits= c(-1, 1),name = "r") +
  scale_x_discrete(expand = c(0,0), labels = c(expression(WS[mean]~vs~WD[mean]),
                                               expression(WS[accum]~vs~WD[mean]),
                                               expression(WS[max]~vs~WD[mean]),
                                               expression(WS[min]~vs~WD[mean]))) +
  scale_y_discrete(expand = c(0,0)) +
  labs(x = 'comparison',y = "well", title = paste('Groups: ',paste0(1:4,' [',grupos_title,']', collapse = "; "))) +
  facet_grid(rows = vars(gr), space = 'free_y', scales = 'free_y', switch = 'y',
             labeller = as_labeller(setNames(1:4,sort(unique(grupos$gr))))) +
  theme_bw() +
  theme(strip.background = element_rect(fill='white'),
        plot.title = element_text(hjust = 0.5))

ggsave(glue('output/fig/water_correlation.png'),height = 8, width = 8)
    

cod <- metricas |> 
  filter(WS_acum_cor_m_prom > .5) |> 
  pull(codigo)

plot(well)

well |> 
  filter(codigo %in% cod) |> 
  plot(add=T,col='red')

well |> 
  left_join(metricas |> select(codigo,WS_acum_cor_m_prom)) |> 
  writeVector('data/processed/vectorial/pozos_cor.shp')



data_anual |> 
  mutate(cor_value = ifelse(!(codigo %in% cod), 'positive','negative')) |> 
  ggplot(aes(año,m_prom,color = cor_value)) +
  geom_hline(yintercept = 0, linetype = 'dashed',alpha = .6) +
  # geom_point() +
  geom_line(aes(group=codigo),alpha = .5,linewidth = .8) +
  # geom_smooth(span=.3, color = 'black') +
  scale_x_continuous(breaks = seq(2000,2022,by=4),
                     minor_breaks = seq(2000,2022,by=2),
                     expand = c(0,.5)) +
  labs(y = 'anual mean well depth',
       x = NULL) +
  theme_bw()




metricas_anual |> 
  summarise(cor_anual = cor(WS_anual, m_fin, use="pairwise"),
            cor_sd = cor(WS_sd, m_fin, use="pairwise"),
            cor_rango = cor(WS_max-WS_min, m_fin, use="pairwise"))



set.seed(1)
x <- c(-10, -2, 0, 1, 3, 20)
x_s <- scale(x) 

plot(x)

x_ss <- scale(x, center = FALSE, scale = TRUE)

plot(x) +
  abline(h = 0) 

plot(x_s) +
  abline(h = 0) +
  points(x_ss,col='red')


# versión dos

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

data_cor <- data_anual |>
  group_by(codigo) |>
  reframe(
    cor_mat = list(
      cor(
        cbind(SSI_WS, WSplus, WSminus, WSrange, WSdelta),
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
  reframe(max_abs_r = max(abs(r), na.rm = TRUE)) |>
  group_by(codigo) |>
  slice_max(order_by = max_abs_r, n = 1) |>
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
                           'WSrange vs WDrange')) |> 
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
