library(tidyverse)
library(terra)
library(glue)

# preprocesar variables

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

# extraer

well_depth <- read_rds('data/processed/rds/well_depth.rds')
well <- vect('data/processed/vectorial/pozos.shp')

tc_var <- c('PPT','AET','Q','SOIL','WS','WS_SM')

data <- lapply(tc_var,\(var) {
  
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
  left_join(well_depth)

data <- data |> 
  mutate(delta_soil = soil-lag(soil,1),
         .before=ws)

write_rds(data,'data/processed/rds/water_balance.rds')

#
data
max(data$ws,na.rm=T)
min(data$ws,na.rm=T)

data |> 
  filter(year(fecha) >= 2000,
         codigo %in% pozos_estacionales) |> 
  ggplot(aes(fecha,ws)) +
  geom_line() +
  facet_wrap(~codigo,ncol=2) +
  labs(y = 'monthly water storage (mm)', x = NULL) +
  theme_bw() +
  theme(strip.background = element_rect(fill = 'white'))

data |> 
  filter(!is.na(ws)) |> 
  group_by(codigo) |> 
  mutate(ws_accum = cumsum(ws)) |> 
  filter(year(fecha) >= 2000,
         codigo %in% pozos_estacionales) |> 
  ggplot(aes(fecha,ws_accum)) +
  geom_line() +
  facet_wrap(~codigo,ncol=2) +
  labs(y = 'monthly water storage accumulated (mm)', x = NULL) +
  theme_bw() +
  theme(strip.background = element_rect(fill = 'white'))

