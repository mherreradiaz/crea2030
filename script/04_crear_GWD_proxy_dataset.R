library(tidyverse)
library(terra)
library(glue)

accum <- \(x,na.use=T) {
  if (na.use==T) {
    id <- which(is.na(x))
    diffs <- c(0, diff(id))
    breaks <- cumsum(diffs != 1)
    first_na <- which(breaks == 1)
    x[first_na] <- 0
    cum <- cumsum(x)
    cum[first_na] <- NA
    return(cum)
  } else if (na.use == 'zero') {
    x[which(is.na(x))] <- 0
    cumsum(x)
  } else if (na.use == F) {cumsum(x) }
  else {
    stop('Valor incorrecto para el argumento na.use. Debe contener TRUE, FALSE o zero')
  }
}
cleanNA <- \(x) x[which(!is.na(x))]
consMean <- \(x,consistency = 1) ifelse(length(cleanNA(x))/length(x) >= consistency,mean(x,na.rm=T),NA)

well <- vect('data/processed/vectorial/pozos/pozos.shp')

# extraer TerraClimate

tc_var <- c('PPT','AET','Q','SOIL','WS','WS_SM')

data_TC_raw <- lapply(tc_var,\(var) {
  
  r <- list.files(glue('data/processed/raster/TerraClimate/{var}'),full.names=T) |> 
    grep(pattern = '.aux',invert=T,value=T) |> 
    rast()
  
  extract(r,well) |> 
    mutate(codigo = well$codigo,
           .before = ID) |> 
    select(-ID) |> 
    pivot_longer(c(everything(),-codigo), names_to = 'fecha',
                 values_to = tolower(var)) |> 
    mutate(fecha = as.Date(paste0(fecha,'-01'))) |> 
    select(fecha,codigo,everything())
  
}) |> 
  reduce(left_join) |> 
  suppressMessages() |>
  mutate(delta_soil = soil-lag(soil,1),
         .before=ws) |> 
  rename(WS = ws,WS_SM = ws_sm) |> 
  arrange(codigo,fecha)

data_TC <- data_TC_raw |> 
  group_by(codigo) |> 
  mutate(WS_acum = cumsum(WS),
         WS_lag3 = lag(WS,3),
         WS_lag6 = lag(WS,6),
         WS_lag12 = lag(WS,12),
         WS_lag3_acum = lag(WS_acum,3),
         WS_lag6_acum = lag(WS_acum,6),
         WS_lag12_acum = lag(WS_acum,12),
         WS_SM_acum = accum(WS_SM),
         WS_SM_lag3 = lag(WS_SM,3),
         WS_SM_lag6 = lag(WS_SM,6),
         WS_SM_lag12 = lag(WS_SM,12),
         WS_SM_lag3_acum = lag(WS_SM_acum,3),
         WS_SM_lag6_acum = lag(WS_SM_acum,6),
         WS_SM_lag12_acum = lag(WS_SM_acum,12)) |> 
  ungroup()

write_rds(data_TC,'data/processed/rds/terraclimate.rds')

# extraer SPI

spi_r <- list.files('data/processed/raster/SPI/',full.names=T) |> 
  rast()

data_SPI_raw <- extract(spi_r,well) |> 
  mutate(codigo = well$codigo,
         .before = ID) |> 
  select(-ID) |> 
  pivot_longer(c(everything(),-codigo), names_to = 'fecha',
               values_to = 'SPI_36') |> 
  mutate(fecha = as.Date(paste0(fecha,'-01'))) |> 
  select(fecha,codigo,everything())

data_SPI <- data_SPI_raw |> 
  rename(SPI = SPI_36) |> 
  arrange(codigo,fecha)

write_rds(data_SPI,'data/processed/rds/SPI.rds')

# unificar dataset

data_TC <- read_rds('data/processed/rds/terraclimate.rds')
data_SPI <- read_rds('data/processed/rds/SPI.rds')
data_GWD <- read_rds('data/processed/rds/well_depth.rds')

data_mes <- left_join(data_TC,data_SPI) |> 
  left_join(data_GWD) |> 
  select(fecha,codigo,GWD,contains('WS'),contains('SPI'))

write_rds(data_mes,'data/processed/rds/GWD_proxy_mes.rds')

data_año <- data_mes |> 
  group_by(codigo,año = year(fecha)) |>
  reframe(
    # GWD
    GWD_mean   = consMean(GWD, .5),
    # WS
    WS_sum          = sum(WS),
    WS_lag3_sum     = sum(WS_lag3),
    WS_lag6_sum     = sum(WS_lag6),
    WS_lag12_sum    = sum(WS_lag12),
    WS_SM_sum       = sum(WS_SM),
    WS_SM_lag3_sum  = sum(WS_SM_lag3),
    WS_SM_lag6_sum  = sum(WS_SM_lag6),
    WS_SM_lag12_sum = sum(WS_SM_lag12),
    WS_acum         = WS_acum[month(fecha)==12],
    WS_lag3_acum    = WS_lag3_acum[month(fecha)==12],
    WS_lag6_acum    = WS_lag6_acum[month(fecha)==12],
    WS_lag12_acum   = WS_lag12_acum[month(fecha)==12],
    WS_SM_acum      = WS_SM_acum[month(fecha)==12],
    WS_SM_lag3_acum = WS_SM_lag3_acum[month(fecha)==12],
    WS_SM_lag6_acum = WS_SM_lag6_acum[month(fecha)==12],
    WS_SM_lag12_acum = WS_SM_lag12_acum[month(fecha)==12],
    # SPI
    SPI_anual = SPI[month(fecha)==12],
    SPI_mean = consMean(SPI,8/12)
  )|>
  filter(between(año,2000,2021))

write_rds(data_año,'data/processed/rds/GWD_proxy_año.rds')

