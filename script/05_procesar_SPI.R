library(tidyverse)
library(terra)
library(glue)

# preprocesar SPI

spi_files <- list.files('data/raw/raster/SPI/',full.names = T)

fechas <- str_extract()

cuenca <- vect('data/processed/vectorial/sitio/cuenca.shp')

spi <- rast(spi_files) |> 
  project('EPSG:32719') |> 
  crop(cuenca) |> 
  setNames(fechas)

dir.out <- 'data/processed/raster/SPI/'

lapply(spi,\(ly) writeRaster(ly,glue('{dir.out}SPI_{names(ly)}.tif')))

# extraer SPI

well_depth <- read_rds('data/processed/rds/well_depth.rds')
well <- vect('data/processed/vectorial/pozos/pozos.shp')

spi_r <- list.files('data/processed/raster/SPI/',full.names=T) |> 
  rast()

extract(spi_r,well) |> 
  mutate(codigo = well$codigo,
         .before = ID) |> 
  select(-ID) |> 
  pivot_longer(c(everything(),-codigo), names_to = 'fecha',
               values_to = 'SPI_36') |> 
  mutate(fecha = as.Date(paste0(fecha,'-01'))) |> 
  select(fecha,codigo,everything()) |> 
  left_join(well_depth) |> 
  rename(spi = SPI_36, GWD = gw_depth) |>
  group_by(codigo) |> 
  mutate(SPI_lag3 = lag(SPI,3),
         SPI_lag6 = lag(SPI,6),
         SPI_lag12 = lag(SPI,12)) |> 
  ungroup() |> 
  write_rds('data/processed/rds/GWD_proxy_SPI.rds')

