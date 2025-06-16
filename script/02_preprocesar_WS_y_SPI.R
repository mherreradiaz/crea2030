library(tidyverse)
library(terra)
library(glue)

# preprocesar TerraClimate

cuenca <- vect('data/processed/vectorial/sitio/cuenca.shp')

tc <- list.files('data/raw/raster/TerraClimate',full.names=T)

lapply(tc, \(tc_var) {
  n <- str_extract(tc_var,'(?<=terraclimate_)[a-z]+(?=_)')
  var <- rast(tc_var) |> 
    project('EPSG:32719',method='bilinear') |> 
    crop(cuenca)
  dir.out <- glue('data/processed/raster/TerraClimate/{toupper(n)}/')
  if (!dir.exists(dir.out)) {
    dir.create(dir.out, recursive = TRUE, showWarnings = FALSE)
  }
  lapply(var, \(ly) {
    date <- time(ly)
    names(ly) <- date
    writeRaster(ly,paste0(dir.out, glue('{toupper(n)}_{substr(date,1,4)}-{substr(date,6,7)}.tif')),
                overwrite=T)
  })
})

# estimar water storage

p <- list.files('data/processed/raster/TerraClimate/PPT',full.names=T) |> 
  grep(pattern = '.aux',invert=T,value=T) |> 
  rast()
et <- list.files('data/processed/raster/TerraClimate/AET',full.names=T) |> 
  grep(pattern = '.aux',invert=T,value=T) |> 
  rast()
q <- list.files('data/processed/raster/TerraClimate/Q',full.names=T) |> 
  grep(pattern = '.aux',invert=T,value=T) |> 
  rast()
s <- list.files('data/processed/raster/TerraClimate/SOIL',full.names=T) |> 
  grep(pattern = '.aux',invert=T,value=T) |> 
  rast()

delta_s <- s-c(setValues(s[[1]],NA), s[[1:(nlyr(s)-1)]])

ws <- p-et-q
ws_sm <- p-et-q-delta_s

lapply(ws_sm, \(ly) {
  date <- names(ly)
  dir.out <- glue('data/processed/raster/TerraClimate/WS_SM/')
  if (!dir.exists(dir.out)) {
    dir.create(dir.out, recursive = TRUE, showWarnings = FALSE)
  }
  writeRaster(ly,paste0(dir.out, glue('WS_SM_{substr(date,1,4)}-{substr(date,6,7)}.tif')), overwrite=T)
})

# preprocesar SPI

spi_files <- list.files('data/raw/raster/SPI/',full.names = T)
cuenca <- vect('data/processed/vectorial/sitio/cuenca.shp')

fechas <- str_extract(spi_files, "\\d{4}-\\d{2}-\\d{2}")

spi <- rast(spi_files) |> 
  project('EPSG:32719') |> 
  crop(cuenca) |> 
  setNames(fechas) |> 
  subset(which(fechas >= '2000-01-01'))

dir.out <- 'data/processed/raster/SPI/'

lapply(spi,\(ly) writeRaster(ly,glue('{dir.out}SPI_{names(ly)}.tif'),
                             overwrite=T))
