library(tidyverse)

data <- read_rds('data/processed/rds/well_depth_aconcagua.rds') |> 
  filter(year(fecha) >= 2000)

# consistency

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

# trend

library(zyp)
library(Kendall)

mk <- function(x,y) {
  x_complete <- seq(min(x),max(x))
  y <- tibble(x = x_complete) |> 
    left_join(tibble(x,y)) |> 
    suppressMessages() |> 
    pull(y)
  tau = as.numeric(MannKendall(y)$tau)
  sl = as.numeric(MannKendall(y)$sl)
  
  return(list('tau' = tau, 'sl' = sl))
}
sen <- function(x,y) {
  data_sen <- tibble(x,y)
  ss <- as.numeric(zyp.sen(y ~ x,data=data_sen)$coefficients)
  
  return(list('int' = ss[1], 'slope' = ss[2]))
}

data_trend <- data_pozos |> 
  na.omit() |>
  group_by(shac,codigo,estacion) |>
  reframe(mk_p_estacional = mk(año,m)$sl,
          sen_slope_estacional = sen(año,m)$slope) |> 
  group_by(shac,codigo) |> 
  reframe(mk_p = max(mk_p_estacional,na.rm=T),
          sen_slope = median(sen_slope_estacional,na.rm=T)) |>
  mutate(class = cut(sen_slope,seq(-1.5,.5,by=.5),right=F,labels=F))

data_trend <- data |> 
  group_by(codigo,año = year(fecha)) |> 
  reframe(m = mean(m,na.rm=T)) |> 
  group_by(codigo) |> 
  
  












codigos_seleccionados <- data_filter |> 
  filter(pt >= pt_min) |> 
  pull(codigo)

pozos <- read_rds('data/processed/rds/pozos_aconcagua.rds') |> 
  filter(codigo %in% codigos_seleccionados)

write_rds(pozos, 'data/processed/rds/pozos.rds')

read_rds('data/processed/rds/well_depth_aconcagua.rds') |> 
  filter(codigo %in% codigos_seleccionados) |> 
  write_rds('data/processed/rds/well_depth.rds')

pozos |> 
  vect(geom = c("lon", "lat"), crs = "EPSG:4326") |> 
  project('EPSG:32719') |> 
  mutate(codigo =as.integer(codigo)) |> 
  writeVector('data/processed/vectorial/pozos.shp',overwrite=T)


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

