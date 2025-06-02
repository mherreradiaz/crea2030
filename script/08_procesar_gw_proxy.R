library(tidyverse)
library(tidyterra)
library(terra)

ws <- list.files('data/processed/raster/TerraClimate/WS',full.names=T) |> 
  grep(pattern = '.aux',value=T,invert=T) |> 
  rast() |> 
  subset(-1)
ws_sm <- list.files('data/processed/raster/TerraClimate/WS_SM',full.names=T) |> 
  grep(pattern = '.aux',value=T,invert=T) |> 
  rast() |> 
  subset(-1)

años <- unique(substr(names(ws),1,4))
dic_idx <- grep('-12-',names(ws))

ws_cumsum <- ws |>
  cumsum() |> 
  subset(dic_idx) |> 
  setNames(años)

ws_sm_cumsum <- ws_sm |>
  cumsum() |> 
  subset(dic_idx) |> 
  setNames(años)

writeRaster(ws_cumsum[[names(ws_cumsum) >= 2000]],
            'data/processed/raster/GW_proxy/WS_acum.tif',
            overwrite=T)
writeRaster(ws_sm_cumsum[[names(ws_sm_cumsum) >= 2000]],
            'data/processed/raster/GW_proxy/WS_SM_acum.tif',
            overwrite=T)

# visualizar

ws <- rast('data/processed/raster/GW_proxy/WS_acum.tif') |> 
  {\(.) subset(., between(as.numeric(names(x=.)),2020,2023))}()
  
ws_sm <- rast('data/processed/raster/GW_proxy/WS_SM_acum.tif') |> 
  {\(.) subset(., between(as.numeric(names(x=.)),2020,2023))}()

cuenca <- vect('data/processed/vectorial/sitio/cuenca.shp')

mask(ws,cuenca) |>
  app(\(x) {
    x[which(!between(x,-20,10))] <- NA
    return(x)}) |> 
  plot()




