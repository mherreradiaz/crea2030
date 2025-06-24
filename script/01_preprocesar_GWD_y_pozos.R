library(tidyverse)
library(terra)
library(tidyterra)
library(readxl)

# Chile ####

data_raw <- read_xlsx('data/raw/tabulada/gw_chile.xlsx') |> 
  select(fecha = Date_String,cuenca = Basin, codigo = Code, GWD = `Depth to water (m)`) |> 
  mutate(fecha = as.Date(fecha),
         codigo = as.integer(codigo)) |> 
  group_by(cuenca,codigo,año = year(fecha), mes = month(fecha)) |> 
  reframe(GWD = -mean(GWD,na.rm=T)) |> 
  mutate(fecha = as.Date(paste(año,mes,'01',sep='-'))) |> 
  select(fecha,cuenca,codigo,GWD) |> 
  filter(!is.na(codigo)) |> 
  mutate(GWD = ifelse(is.na(GWD)|GWD > 0,NA,GWD))

data <- tibble(codigo = unique(data_raw$codigo)) |> 
  group_by(codigo) |> 
  reframe(fecha = seq.Date(as.Date('1980-01-01'),as.Date('2021-12-01'),by='month')) |> 
  left_join(data_raw) |> 
  select(fecha,cuenca,codigo,GWD)

write_rds(data,'data/processed/rds/GWD_chile.rds')

read_xlsx('data/raw/tabulada/gw_chile.xlsx') |> 
  select(cuenca = Basin, codigo = Code, lon = Longitude_GCS_WGS_1984, lat = Latitude_GCS_WGS_1984) |> 
  mutate(codigo = as.integer(codigo)) |> 
  filter(codigo %in% data$codigo) |> 
  distinct() |> 
  arrange(desc(lat)) |> 
  write_rds('data/processed/rds/pozos_chile.rds')

read_rds('data/processed/rds/pozos_chile.rds') |> 
  vect(geom = c("lon", "lat"), crs = "EPSG:4326") |> 
  mutate(codigo =as.integer(codigo)) |> 
  writeVector('data/processed/vectorial/pozos/pozos_chile.shp',overwrite=T)

# Aconcagua ####

data <- read_rds('data/processed/rds/GWD_chile.rds') |> 
  filter(cuenca == 'RIO ACONCAGUA',
         year(fecha) >= 2000)

# Consistencia pozos

# data_filter <- data |> 
#   group_by(codigo,año = year(fecha)) |> 
#   reframe(head = sum(!is.na(head(GWD,3))), # valores !NA en los primeros tres meses (máximo 3)
#           tail = sum(!is.na(tail(GWD,3))), # valores !NA en los últimos tres meses (máximo 3)
#           total = sum(!is.na(GWD))) |>  # meses con valores !NA (máximo 12)
#   rowwise() |> 
#   mutate(head_tail = sum(head >= 1 & tail >= 1)) |> # valores !NA entre primeros y últimos tres meses (en) 
#   group_by(codigo) |>
#   reframe(año_mt_6 = sum(total >= 6), # n de años con más de 6 meses
#           año_mt_4 = sum(total >= 4), # n de años con más de 4 meses
#           head_mt_1 = sum(head >= 1), # n de años con al menos un valor en los primeros tres meses
#           tail_mt_1 = sum(tail >= 1), # n de años con al menos un valor en los ultimos tres meses
#           head_tail_1 = sum(head_tail >= 1)) |> # n de años con al menos un valor en los primeros y ultimos tres meses
#   rowwise() |> 
#   mutate(pt = año_mt_6*.25+año_mt_4*.2+head_mt_1*.15+tail_mt_1*.15+head_tail_1*.25)
# 
# pt_min <- data_filter |> 
#   pull(pt) |> 
#   quantile(.57)
# 
# codigos_seleccionados <- data_filter |> 
#   filter(pt >= pt_min) |> 
#   pull(codigo)

data_n <- data |> 
  na.omit() |> 
  group_by(codigo,año = year(fecha)) |> 
  reframe(n = n()) |> 
  filter(n >= 3) |> 
  group_by(codigo) |> 
  reframe(n = n())

codigos_seleccionados <- data_n |> 
  filter(n >= ceiling(22*.75)) |> 
  pull(codigo)

pozos <- read_rds('data/processed/rds/pozos_chile.rds') |> 
  filter(codigo %in% codigos_seleccionados)

write_rds(pozos,'data/processed/rds/pozos_aconcagua.rds')

# filtrar

read_rds('data/processed/rds/GWD_chile.rds') |> 
  filter(codigo %in% codigos_seleccionados) |> 
  write_rds('data/processed/rds/GWD_aconcagua.rds')

vect('data/processed/vectorial/pozos/pozos_chile.shp') |>
  filter(codigo %in% codigos_seleccionados) |> 
  writeVector('data/processed/vectorial/pozos/pozos_aconcagua.shp',overwrite=T)

# fill data (no aplicado)

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

data <- read_rds('data/processed/rds/GWD_aconcagua.rds') |> 
  filter(year(fecha) >= 2000)

codigos_seleccionados <- read_rds('data/processed/rds/pozos_seleccionados.rds') |> 
  pull(codigo)


