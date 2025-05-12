library(tidyverse)
library(tidyterra)
library(terra)

# data

well_depth <- read_rds('data/processed/rds/well_depth.rds')

well <- read_rds('data/processed/rds/pozos.rds') |> 
  vect(geom = c("lon", "lat"), crs = "EPSG:4326") |> 
  project('EPSG:32719') |> 
  mutate(codigo =as.integer(codigo))

writeVector(well,'data/processed/vectorial/pozos.shp')

ws_r <- list.files('data/processed/raster/TerraClimate/WS',full.names=T) |> 
  rast()

ws <- extract(ws_r,well) |> 
  mutate(codigo = well$codigo,
         .before = ID) |> 
  select(-ID) |> 
  pivot_longer(c(everything(),-codigo), names_to = 'fecha',
               values_to = 'ws') |> 
  mutate(fecha = as.Date(paste0(fecha,'-01'))) |> 
  select(fecha,codigo,ws)

data <- left_join(ws,well_depth)

write_rds(data,'data/processed/rds/water_storage.rds')

# correlation

library(patchwork)

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

data_anual |> #variables por pozo
  filter(codigo == well$codigo[i]) |>
  pivot_longer(cols=c(delta_m,m_prom,WS_acum,WS_anual),names_to='var',values_to='value') |> 
  ggplot(aes(año,value,color=var)) +
  geom_point() +
  geom_line(alpha = .8,linewidth = .8) +
  geom_hline(yintercept = 0, linetype = 'dashed',alpha = .6) +
  scale_x_continuous(breaks = seq(2000,2024,by=4),
                     minor_breaks = seq(2000,2024,by=2),
                     expand = c(0,.5)) +
  labs(y = 'scaled value',
       x = NULL,
       color = 'variable') +
  scale_color_discrete(labels = c('Δwd','mean wd','accum S','mean S')) +
  guides(color = guide_legend(reverse = TRUE)) +
  theme_bw()

p1 <- data_anual |> # WS_anual
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

(p1 + p2) / (p3 + p4)

ggsave('output/fig/water.png',height = 7, width = 11)

library(RColorBrewer)

metricas <- data_anual |> 
  # group_by(codigo) |> 
  summarise(WS_anual_cor_m_prom = cor(WS_anual, m_prom, use='complete.obs'),
            WS_anual_cor_delta_m = cor(WS_anual, delta_m, use='complete.obs'),
            WS_acum_cor_m_prom = cor(WS_acum, m_prom, use='complete.obs'),
            WS_acum_cor_delta_m = cor(WS_acum, delta_m, use='complete.obs')) |> 
  pivot_longer(cols = -codigo,names_to  = 'comparation',values_to = 'r')

metricas |> 
  ggplot(aes(comparation, y = as.factor(codigo), fill = r)) +
  geom_tile() +
  geom_text(aes(label = round(r, 2)), size = 3) +
  scale_fill_distiller(palette   = "RdBu",direction = 1,limits= c(-1, 1),name = "correlation") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  labs(x = "comparation",y = "well") +
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

