library(tidyverse)

data <- read_rds('data/processed/rds/water_storage.rds') |> 
  filter(year(fecha) >= 1999) |> 
  rename(WS=ws,WD=m) |> 
  group_by(codigo) |> 
  mutate(WS_acum = cumsum(WS),
         WD_lead3 = lead(WD,3),
         WD_lead6 = lead(WD,6),
         WD_lead12 = lead(WD,12)) |> 
  ungroup()

data |> 
  group_by(codigo,año = year(fecha)) |> 
  mutate(WD = as.numeric(scale(WD,center=F))) |> 
  group_by(codigo, mes = month(fecha)) |> 
  reframe(WD = mean(WD,na.rm=T)) |> 
  # filter(codigo %in% unique(data$codigo)[15:27]) |> 
  ggplot(aes(mes,WD)) +
  geom_point() +
  geom_line(linetype = 'dashed',col = 'red') +
  geom_smooth() +
  scale_x_continuous(breaks = 1:12) +
  facet_wrap(~codigo,ncol=9) +
  theme_bw()

pozos_estacionales <- c(5423017,5423023,5425006,5426005,5426006,
                        5426008,5426015,5426016,5426020,542622,5428008)

data_cor <- data |> 
  group_by(codigo) |> 
  reframe(across(
    .cols = c(WD, WD_lead3, WD_lead6, WD_lead12),
    .fns  = ~ cor(WS_acum, ., use = "pairwise.complete.obs"),
    .names = "WS_vs_{.col}")) |> 
  filter(codigo %in% pozos_estacionales)

order <- data_cor |> 
  mutate(cor_mean = rowMeans(across(-1), na.rm = TRUE)) |> 
  arrange(desc(cor_mean)) |> 
  pull(codigo)

data |> 
  select(fecha,codigo,WS_acum,WD) |> 
  pivot_longer(cols=c(WS_acum,WD),values_to='value',names_to='var') |> 
  group_by(codigo,var) |> 
  mutate(value = as.numeric(scale(value,center=F))) |> 
  filter(codigo %in% unique(data$codigo)[1:6]) |>
  na.omit() |> 
  ggplot(aes(fecha,value,color=var)) +
  geom_line() +
  facet_wrap(~codigo,ncol=3) +
  theme_bw()
  
