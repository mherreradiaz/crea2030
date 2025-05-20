library(tidyverse)
library(readxl)

data_raw <- read_xlsx('data/raw/tabulada/gw_chile.xlsx') |> 
  filter(Basin == 'RIO ACONCAGUA') |> 
  select(fecha = Date_String,codigo = Code, m = `Depth to water (m)`) |> 
  mutate(fecha = as.Date(fecha),
         codigo = as.integer(codigo)) |> 
  group_by(codigo,año = year(fecha), mes = month(fecha)) |> 
  reframe(m = -mean(m,na.rm=T)) |> 
  mutate(fecha = as.Date(paste(año,mes,'01',sep='-'))) |> 
  select(codigo,fecha,m)

data <- tibble(codigo = unique(data_raw$codigo)) |> 
  group_by(codigo) |> 
  reframe(fecha = seq.Date(as.Date('1980-01-01'),as.Date('2021-12-01'),by='month')) |> 
  left_join(data_raw) |> 
  mutate(m = ifelse(is.na(m),NA,m)) |> 
  filter(m <= 0)
  
write_rds(data,'data/processed/rds/well_depth_aconcagua.rds')

read_xlsx('data/raw/tabulada/gw_chile.xlsx') |> 
  filter(Basin == 'RIO ACONCAGUA') |> 
  select(codigo = Code, lon = Longitude_GCS_WGS_1984, lat = Latitude_GCS_WGS_1984) |> 
  mutate(codigo = as.integer(codigo)) |> 
  distinct() |> 
  write_rds('data/processed/rds/pozos_aconcagua.rds')

