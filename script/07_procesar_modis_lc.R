library(tidyverse)
library(tidyterra)
library(terra)
library(parallel)

terraOptions(threads = detectCores()-2)

# procesar MOD12Q1

files <- list.files('data/raw/raster/MOD12Q1/',full.names=T,pattern='.tif') |> 
  grep(pattern='Type1',value=T)

years <- str_extract(files,"(?<=doy)\\d{4}")

r <- files |> 
  rast() |> 
  project('EPSG:32719',method = 'near') |> 
  setNames(years) |> 
  subset(19)

writeRaster(r,'data/processed/raster/cobertura/MOD12Q1.tif',
            overwrite=T)

# validar LC con catastro CONAF

mod12 <- rast('data/processed/raster/cobertura/MOD12Q1.tif')
conaf <- vect('data/processed/vectorial/cobertura/forestal.shp')

mod12_disag <- disagg(mod12,41)

conaf_rast <- conaf |> 
  rasterize(mod12_disag,field='CLASS',fun = sum)

conaf_rast <- ifel(!(conaf_rast %in% unique(conaf$CLASS)),NA,conaf_rast)

data <- c(conaf_rast,mod12_disag) |> 
  setNames(c('CONAF','MODIS')) |> 
  values() |> 
  as_tibble()

# omitir NA

conf_matrix <- data |>
  na.omit() |> 
  group_by(CONAF, MODIS) |>
  reframe(n = n()) |>
  pivot_wider(
    names_from   = MODIS,
    values_from  = n,
    values_fill  = 0
  ) |>
  tibble::column_to_rownames('CONAF')

conf_matrix

# considerar NA

conf_matrix_na <- data |>
  mutate(CONAF = ifelse(is.na(CONAF),-999,CONAF),
         MODIS = ifelse(is.na(MODIS),-999,MODIS)) |> 
  group_by(CONAF,MODIS) |>
  reframe(n = n()) |>
  pivot_wider(
    names_from   = MODIS,
    values_from  = n,
    values_fill  = 0
  ) |>
  tibble::column_to_rownames('CONAF')

conf_matrix_na
