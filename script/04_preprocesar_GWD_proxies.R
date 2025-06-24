library(tidyverse)
library(terra)
library(glue)

exportRast <- \(r, output.dir, band.name, names = TRUE) {
  require(terra)
  require(glue)
  
  if (!dir.exists(output.dir)) dir.create(output.dir, recursive = TRUE)
  if (!grepl('/$', output.dir)) output.dir <- glue('{output.dir}/')
  
  if (is.logical(names) && names == TRUE) {
    name_list <- names(r)
    if (is.null(name_list) || any(is.na(name_list))) {
      stop('El objeto raster no tiene nombres de capa definidos.')
    }
  } else if (is.character(names) && length(names) == nlyr(r)) {
    name_list <- names
  } else {
    stop("El argumento 'names' debe ser TRUE o un vector de caracteres de largo igual al número de capas.")
  }
  
  file_names <- gsub('//','/',glue('{output.dir}{band.name}{name_list}.tif'))
  
  preview <- head(file_names, 10)
  cat('Las capas se guardarán de la siguiente forma (10 primeras):\n')
  cat(paste0(preview, collapse = '\n'), '\n')
  
  respuesta <- readline(prompt = '¿Desea proceder con la exportación? [s/n]: ')
  if (tolower(respuesta) != 's') {
    cat('Exportación cancelada.\n')
    return(invisible(NULL))
  }
  
  lapply(1:nlyr(r), \(i) {
    writeRaster(r[[i]], file_names[i], overwrite = TRUE)
  })
  
  cat('Exportación finalizada.\n')
}

# TerraClimate ####

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

dir.out <- glue('data/processed/raster/TerraClimate/')
delta_s

# exportRast(delta_s,glue('{dir.out}/deltaSM'),band.name = 'deltaSM_',names = substr(names(delta_s),1,7))
# exportRast(p-et,glue('{dir.out}/P_ET'),band.name = 'P_ET_',names = substr(names(p),1,7))

# SPI ####

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

