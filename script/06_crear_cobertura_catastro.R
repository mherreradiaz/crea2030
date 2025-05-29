library(tidyverse)
library(tidyterra)
library(terra)
library(parallel)

an <- \(x) {ifelse(is.na(x),0,as.numeric(x))}
terraOptions(threads = detectCores()-2)
modal_per <- function(x) {
  if (length(which(!is.na(x))) == 0) NA 
  else {
    l = length(x)
    t = table(x)
    n <- max(t)
    prop <- n/l
    if (prop >= .7) as.numeric(names(t)[which.max(t)]) else NA
  }
}

# procesar catastro forestal

v <- grep('forestal',list.files('data/raw/vectorial',full.names=T,pattern = '.shp'),value=T)[3] |> 
  vect() |>
  mutate(CLASS = an(ID_USO)*100000 + an(ID_SUBUSO)*10000 + an(ID_ESTRUC)*1000 + an(ID_COBER)*100 + an(ID_TIFO)) |> 
  select(CLASS,USO,SUBUSO,ESTRUCTURA,COBERTURA,TIPO_FORES) |> 
  aggregate(by='CLASS',cores = 10) |> 
  select(-agg_n) |> 
  mutate(CLASS = as.integer(CLASS))

cuenca <- vect('data/processed/vectorial/sitio/cuenca.shp')

v |>
  mutate(LVL_1 = as.integer(substr(CLASS,1,1)),
         LVL_2 = as.integer(substr(CLASS,1,2)),
         LVL_3 = as.integer(substr(CLASS,1,3)),
         LVL_4 = as.integer(substr(CLASS,1,4)),
         LVL_5 = as.integer(substr(CLASS,1,6)),
         .before = USO) |>
  filter(between(LVL_1,3,4.9)) |> 
  crop(cuenca) |> 
  writeVector('data/processed/vectorial/cobertura/forestal.shp',
            overwrite=T)

# cobertura forestal

v <- vect('data/processed/vectorial/cobertura/forestal.shp') |> 
  select(LVL_3) |> values() |> distinct() |> write_csv('clases.csv')

r <- list.files('data/raw/raster/MOD12Q2',full.names=T,pattern = '.tif')[1] |>
  rast() |> 
  setValues(NA) |> 
  project('EPSG:32719')

r_disagg <- disagg(r, fact = 41)

r_lvl1 <- v |> 
  select(LVL_1) |> 
  aggregate(by='LVL_1',cores=10) |> 
  rasterize(r_disagg,field='LVL_1',fun = sum)

r_lvl2 <- v |> 
  select(LVL_2) |> 
  aggregate(by='LVL_2',cores=10) |> 
  rasterize(r_disagg,field='LVL_2',fun = sum)

r_lvl3 <- v |> 
  select(LVL_3) |> 
  aggregate(by='LVL_3',cores=10) |> 
  rasterize(r_disagg,field='LVL_3',fun = sum)

r_lvl4 <- v |> 
  select(LVL_4) |> 
  aggregate(by='LVL_4',cores=10) |> 
  rasterize(r_disagg,field='LVL_4',fun = sum)

r_lvl5 <- v |> 
  select(LVL_5) |> 
  aggregate(by='LVL_5',cores=10) |> 
  rasterize(r_disagg,field='LVL_5',fun = sum)
 
r_lvl1 <- ifel(!(r_lvl1 %in% unique(v$LVL_1)),NA,r_lvl1) |> 
  aggregate(41, fun = modal_per,cores = 10)
r_lvl2 <- ifel(!(r_lvl2 %in% unique(v$LVL_2)),NA,r_lvl2) |> 
  aggregate(41, fun = modal_per,cores = 10)
r_lvl3 <- ifel(!(r_lvl3 %in% unique(v$LVL_3)),NA,r_lvl3) |> 
  aggregate(41, fun = modal_per,cores = 10)
r_lvl4 <- ifel(!(r_lvl4 %in% unique(v$LVL_4)),NA,r_lvl4) |> 
  aggregate(41, fun = modal_per,cores = 10)
r_lvl5 <- ifel(!(r_lvl5 %in% unique(v$LVL_5)),NA,r_lvl5) |> 
  aggregate(41, fun = modal_per,cores = 10)

c(r_lvl1,r_lvl2,r_lvl3,r_lvl4,r_lvl5) |> 
  setNames(paste0('lvl_',1:5)) |> 
  writeRaster('data/processed/raster/cobertura/forestal.tif',
              overwrite=T)

# procesar catastro fruticola

v <- grep('fruticola',list.files('data/raw/vectorial',full.names=T,pattern = '.shp'),value=T)[3] |> 
  vect() |>
  filter(ESPECIE %in% c('PALTO','NOGAL','VID DE MESA','LIMONERO','MANDARINO')) |> 
  mutate(ESPECIE = factor(ESPECIE, levels = c('PALTO','NOGAL','VID DE MESA','LIMONERO','MANDARINO')),
         CLASS = as.integer(ESPECIE)) |> 
  aggregate(by='CLASS',cores = 10) |> 
  select(CLASS)

cuenca <- vect('data/processed/vectorial/sitio/cuenca.shp')

v |>  
  crop(cuenca) |> 
  writeVector('data/processed/vectorial/cobertura/fruticola.shp',
              overwrite=T)

# cobertura fruticola

v <- vect('data/processed/vectorial/cobertura/fruticola.shp')

r <- list.files('data/raw/raster/MOD12Q2',full.names=T,pattern = '.tif')[1] |>
  rast() |> 
  setValues(NA) |> 
  project('EPSG:32719')

r_disagg <- disagg(r, fact = 41)

r_lvl1 <- v |> 
  rasterize(r_disagg,field='CLASS',fun = sum)

ifel(!(r_lvl1 %in% unique(v$CLASS)),NA,r_lvl1) |> 
  aggregate(41, fun = modal_per,cores = 10) |> 
  setNames('lvl_1') |> 
  writeRaster('data/processed/raster/cobertura/fruticola.tif',
              overwrite=T)
