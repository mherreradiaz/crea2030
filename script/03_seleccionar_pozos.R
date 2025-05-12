library(tidyverse)

data <- read_rds('data/processed/rds/well_depth_aconcagua.rds') |> 
  filter(year(fecha) >= 2000)

data_filter <- data |> 
  group_by(codigo,año = year(fecha)) |> 
  reframe(head = sum(!is.na(head(m,3))), # valores !NA en los primeros tres meses (máximo 3)
          tail = sum(!is.na(tail(m,3))), # valores !NA en los últimos tres meses (máximo 3)
          total = sum(!is.na(m))) |>  # meses con valores !NA (máximo 12)
  rowwise() |> 
  mutate(head_tail = sum(head >= 1 & tail >= 1)) |> # valores !NA entre primeros y últimos tres meses (en) 
  group_by(codigo) |>
  reframe(año_mt_6 = sum(total >= 6), # n de años con más de 6 meses
          año_mt_4 = sum(total >= 4), # n de años con más de 4 meses
          head_mt_1 = sum(head >= 1), # n de años con al menos un valor en los primeros tres meses
          tail_mt_1 = sum(tail >= 1), # n de años con al menos un valor en los ultimos tres meses
          head_tail_1 = sum(head_tail >= 1)) |> # n de años con al menos un valor en los primeros y ultimos tres meses
  rowwise() |> 
  mutate(pt = año_mt_6*.25+año_mt_4*.2+head_mt_1*.15+tail_mt_1*.15+head_tail_1*.25)

pt_min <- data_filter |> 
  pull(pt) |> 
  quantile(.57)

codigos_seleccionados <- data_filter |> 
  filter(pt >= pt_min) |> 
  pull(codigo)

read_rds('data/processed/rds/pozos_aconcagua.rds') |> 
  filter(codigo %in% codigos_seleccionados) |> 
  write_rds('data/processed/rds/pozos.rds')

read_rds('data/processed/rds/well_depth_aconcagua.rds') |> 
  filter(codigo %in% codigos_seleccionados) |> 
  write_rds('data/processed/rds/well_depth.rds')

# fill data

library(zoo)

na.str <- \(x) as.numeric(na.omit(x))
fill_zoo <- \(x) {
  
  if (sum(!is.na(x)) < 5) {return(x)} else {
    f <- first(which(!is.na(x)))
    l <- last(which(!is.na(x)))
    
    df <- tibble(id = seq_along(x), x)
    fill_x <- na.approx(x)
    df_filled <- tibble(id = seq(f,l), fill_x)
    
    left_join(df,df_filled) |> 
      suppressMessages() |> 
      pull(fill_x)
  }
}

data <- read_rds('data/processed/rds/well_depth_aconcagua.rds') |> 
  filter(year(fecha) >= 2000)

codigos_seleccionados <- read_rds('data/processed/rds/pozos_seleccionados.rds') |> 
  pull(codigo)

data |> 
  filter(codigo %in% codigos_seleccionados[c(1,4,8,12,16,20)]) |> 
  group_by(codigo) |> 
  mutate(m_filled = fill_zoo(m)) |> 
  ggplot(aes(fecha,m_filled,color = 'm_filled')) +
  geom_point() +
  geom_line() +
  geom_point(aes(fecha,m,color ='m')) +
  geom_line(aes(fecha,m,color ='m')) +
  scale_x_date(date_labels = "%Y",date_breaks = '2 years') +
  facet_wrap(~codigo,ncol=2,scale = 'free_y') +
  theme_bw()

