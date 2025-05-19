library(tidyverse)
library(terra)
library(glue)

# procesar variables

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
    writeRaster(ly,paste0(dir.out, glue('{toupper(n)}_{substr(date,1,4)}-{substr(date,6,7)}.tif')))
  })
})

# estimar water storage

p <- list.files('data/processed/raster/TerraClimate/PPT',full.names=T) |> 
  rast()
et <- list.files('data/processed/raster/TerraClimate/AET',full.names=T) |> 
  rast()
q <- list.files('data/processed/raster/TerraClimate/Q',full.names=T) |> 
  rast()

ws <- p-et-q

names(ws) <- gsub('PPT_|.tif','',basename(sources(p)))

lapply(ws, \(ly) {
  date <- names(ly)
  dir.out <- glue('data/processed/raster/TerraClimate/WS/')
  if (!dir.exists(dir.out)) {
    dir.create(dir.out, recursive = TRUE, showWarnings = FALSE)
  }
  writeRaster(ly,paste0(dir.out, glue('WS_{substr(date,1,4)}-{substr(date,6,7)}.tif')))
})

# extraer

well_depth <- read_rds('data/processed/rds/well_depth.rds')
well <- vect('data/processed/vectorial/pozos.shp')
ws_r <- list.files('data/processed/raster/TerraClimate/WS',full.names=T) |> 
  rast()

ws <- extract(ws_r,well) |> 
  mutate(codigo = well$codigo,
         .before = ID) |> 
  select(-ID) |> 
  pivot_longer(c(everything(),-codigo), names_to = 'fecha',
               values_to = 'ws') |> 
  mutate(fecha = as.Date(paste0(fecha,'-01'))) |> 
  select(fecha,codigo,ws)

data <- left_join(ws,well_depth)

write_rds(data,'data/processed/rds/water_storage.rds')
