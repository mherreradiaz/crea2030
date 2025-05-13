library(tidyverse)
library(tidyterra)
library(terra)
library(RColorBrewer)
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
  pivot_longer(cols = c(WS_acum,WS_min,m_prom),names_to = 'variable',values_to = 'value') |> 
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

