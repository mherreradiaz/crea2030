library(tidyverse)
library(terra)
library(glue)
library(fs)

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
calc_spi_like <- \(x, window = 36) {
  require(zoo)
  roll_sum <- rollapply(x, width = window, FUN = sum, align = 'right', fill = NA, na.rm = FALSE)
  scale(roll_sum)[,1]  # escala z
}

well <- vect('data/processed/vectorial/pozos/pozos_aconcagua.shp')

# extraer TerraClimate

tc_var <- c('PPT','AET','Q','SOIL')

tc_extract <- lapply(tc_var,\(var) {
  
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
  suppressMessages()

data_TC <- tc_extract |>
  mutate(deltaSM = soil-lag(soil,1),
         P_ET = ppt-aet) |> 
  arrange(codigo,fecha) |> 
  rename(P = ppt, ET = aet, Q = q, SM = soil)

write_rds(data_TC,'data/processed/rds/terraclimate.rds')

# extraer SPI

spi_r <- list.files('data/processed/raster/SPI/',full.names=T) |> 
  rast()

SPI_extract <- extract(spi_r,well) |> 
  mutate(codigo = well$codigo,
         .before = ID) |> 
  select(-ID) |> 
  pivot_longer(c(everything(),-codigo), names_to = 'fecha',
               values_to = 'SPI_36') |> 
  mutate(fecha = as.Date(paste0(fecha,'-01'))) |> 
  select(fecha,codigo,everything())

data_SPI <- SPI_extract |> 
  rename(SPI = SPI_36) |> 
  arrange(codigo,fecha)

write_rds(data_SPI,'data/processed/rds/SPI.rds')

# extraer GRACE

grace_files <- dir_ls('data/processed/raster/GRACE_highres/')
fechas <- str_extract(grace_files,'(?<=GRACE_highres_)\\d{4}-\\d{2}')

grace_r <- grace_files|> 
  rast() |>
  setNames(fechas)

GRACE_extract <- extract(grace_r,well) |> 
  mutate(codigo = well$codigo,
         .before = ID) |> 
  select(-ID) |> 
  pivot_longer(c(everything(),-codigo), names_to = 'fecha',
               values_to = 'LWE') |> 
  mutate(fecha = as.Date(paste0(fecha,'-01'))) |> 
  select(fecha,codigo,everything())

data_GRACE <- GRACE_extract |> 
  arrange(codigo,fecha)

write_rds(data_GRACE,'data/processed/rds/GRACE.rds')

# unificar dataset

data_GWD <- read_rds('data/processed/rds/GWD_aconcagua.rds')

data_TC <- read_rds('data/processed/rds/terraclimate.rds')
data_SPI <- read_rds('data/processed/rds/SPI.rds')
data_GRACE <- read_rds('data/processed/rds/GRACE.rds')

data_mes <- data_GWD |> 
  filter(year(fecha) >= 1997) |>
  left_join(data_TC) |>
  left_join(rename(data_SPI,SPI_ERA = SPI)) |> 
  left_join(data_GRACE) |> 
  group_by(codigo) |> 
  mutate(SPI_TC     = calc_spi_like(P),
         WS         = P-ET-Q,
         P_ET       = P-ET,
         WS_SI      = calc_spi_like(WS),
         P_ET_SI    = calc_spi_like(P_ET),
         Q_SI       = calc_spi_like(Q),
         SSI        = calc_spi_like(SM),
         deltaSM_SI = calc_spi_like(deltaSM),
         LWE_SI     = calc_spi_like(LWE))

write_rds(data_mes,'data/processed/rds/GWD_proxy_mes.rds')

data_año <- data_mes |> 
  group_by(codigo,año = year(fecha)) |>
  reframe(
    # GWD
    GWD_mean   = consMean(GWD, 3/12),
    # GRACE
    LWE_mean   = mean(LWE),
    LWE_SI     = LWE_SI[month(fecha)==12],
    # TerraClimate
    P_sum = sum(P),
    P_ET_sum    = sum(P_ET),
    Q_sum       = sum(Q),
    SM_sum       = sum(SM),
    deltaSM_sum = sum(deltaSM),
    SPI_TC     = SPI_TC[month(fecha)==12],
    SSI       = SSI[month(fecha)==12],
    P_ET_SI     = P_ET_SI[month(fecha)==12],
    Q_SI     = Q_SI[month(fecha)==12],
    deltaSM_SI     = deltaSM_SI[month(fecha)==12],
    # ERA-5
    SPI_ERA = SPI_ERA[month(fecha)==12],
  )|>
  filter(año >= 2000)

write_rds(data_año,'data/processed/rds/GWD_proxy_año.rds')

