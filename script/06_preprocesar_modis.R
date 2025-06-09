library(tidyverse)
library(tidyterra)
library(terra)
library(parallel)
library(beepr)
library(glue)

terraOptions(threads = detectCores()-2)

# preprocesar NDVI

cuenca <- vect('data/processed/vectorial/sitio/cuenca.shp')

qa_files <- list.files('data/raw/raster/MOD13Q1',full.names=T,pattern = '.tif') |> 
  grep(pattern = 'VI_Q',value=T) |> 
  grep(pattern = 'aux',value = T, invert = T)

r_qa <- rast(qa_files) |> 
  lapply(\(ly) {
    qa_vals <- values(ly)
    bits <- t(sapply(qa_vals, \(x) as.integer(intToBits(x)[1:16])))
    MODLAND_QA        <- bits[,1] + 2 * bits[,2]
    VI_usefulness     <- bits[,3] + 2 * bits[,4] + 4 * bits[,5] + 8 * bits[,6]
    Aerosol_quantity  <- bits[,7] + 2 * bits[,8]
    Adjacent_cloud    <- bits[,9]
    BRDF_corr         <- bits[,10]
    Mixed_clouds      <- bits[,11]
    Land_water_flag   <- bits[,12] + 2 * bits[,13] + 4 * bits[,14]
    Snow_ice          <- bits[,15]
    Shadow            <- bits[,16]
    r1 <- setValues(ly[[1]], MODLAND_QA)
    r2 <- setValues(ly[[1]], VI_usefulness)
    r3 <- setValues(ly[[1]], Aerosol_quantity)
    r4 <- setValues(ly[[1]], Adjacent_cloud)
    r5 <- setValues(ly[[1]], BRDF_corr)
    r6 <- setValues(ly[[1]], Mixed_clouds)
    r7 <- setValues(ly[[1]], Land_water_flag)
    r8 <- setValues(ly[[1]], Snow_ice)
    r9 <- setValues(ly[[1]], Shadow)
    
    r_stack <- c(r1, r2, r3, r4, r5, r6, r7, r8, r9)
    names(r_stack) <- c("MODLAND_QA", "VI_usefulness", "Aerosol_quantity",
                        "Adjacent_cloud", "BRDF_corr", "Mixed_clouds",
                        "Land_water_flag", "Snow_ice", "Shadow")
    r_stack
  })

qa_mask <- lapply(r_qa, \(ly) {
  mask_calidad <- 
    (ly$MODLAND_QA       %in% c(0,1)) &        
    (ly$VI_usefulness    <= 2)     &           
    (ly$Aerosol_quantity %in% c(0,1)) &     
    (ly$Adjacent_cloud   == 0)     &
    (ly$BRDF_corr        == 0)     &
    (ly$Mixed_clouds     == 0)     &
    (ly$Land_water_flag  == 1)     &      
    (ly$Snow_ice         == 0)     &
    (ly$Shadow           == 0)
  mask_calidad
}) |> 
  rast()

fechas <- tibble(año_doy = str_extract(qa_files, "(?<=doy)\\d{7}")) |> 
  separate(col = año_doy, into = c('año','doy'), sep = 4, convert = T) |> 
  mutate(fecha = ymd(paste0(año, "0101")) + days(doy - 1)) |> 
  pull(fecha)

qa_mask <- setNames(qa_mask,fechas)

ndvi_files <- list.files('data/raw/raster/MOD13Q1',full.names=T,pattern = '.tif') |> 
  grep(pattern = 's_NDVI_d',value=T) |> 
  grep(pattern = 'aux',value = T, invert = T)

r_ndvi <- rast(ndvi_files)

cuenca <- vect('data/processed/vectorial/sitio/cuenca.shp')

ndvi <- r_ndvi |>
  mask(qa_mask,maskvalues=F) |> 
  project('EPSG:32719') |> 
  mask(cuenca) |> 
  setNames(fechas)
beep(8)

dir.out <- 'data/processed/raster/MOD13Q1/NDVI_'

lapply(ndvi,\(ly) 
  writeRaster(ly, glue('{dir.out}{names(ly)}.tif'),
              overwrite=T)
)

# preprocesar MOD12Q1

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

#

data_cont <- conf_matrix_na |> 
  select(-1) |>
  mutate(conaf = row.names(conf_matrix_na),
         .before = `1`,
         conaf = ifelse(conaf == -999,"no_data",conaf)) |> 
  pivot_longer(cols = `1`:`10`,names_to = 'MODIS',values_to = 'n') |> 
  select(MODIS,conaf,n) |> 
  arrange(MODIS) |> 
  mutate(MODIS = as.factor(MODIS),
         conaf = as.factor(conaf))

write_rds(data_cont,'data/processed/rds/data_contingencia_modis_conaf.rds')

#

cuenca <- vect('data/processed/vectorial/sitio/cuenca.shp')

conaf <- vect('data/raw/vectorial/catastro_forestal.shp') |> 
  crop(cuenca)

data <- read_rds('data/processed/rds/data_contingencia_modis_conaf.rds') |> 
  mutate(MODIS = as.numeric(as.character(MODIS))) |> 
  mutate(MODIS = factor(case_when(MODIS == 1  ~ "Bosque siempreverde de coníferas",
                                  MODIS == 2  ~ "Bosque siempreverde de hoja ancha",
                                  MODIS == 6  ~ "Matorrales densos",
                                  MODIS == 7  ~ "Matorrales abiertos",
                                  MODIS == 8  ~ "Sabana arbustiva",
                                  MODIS == 9  ~ "Sabana",
                                  MODIS == 10 ~ "Pastizales",
                                  .default = NULL),
                        levels = c("Bosque siempreverde de coníferas",
                                   "Bosque siempreverde de hoja ancha",
                                   "Matorrales densos",
                                   "Matorrales abiertos",
                                   "Sabana arbustiva",
                                   "Sabana",
                                   "Pastizales")))

data_cont <- data |> 
  group_by(MODIS) |> 
  mutate(pred = n/sum(n))

data_px <- data_cont |> 
  reframe(sum = sum(n))

data_cont |> 
  slice_max(n, n = 4) |> 
  ggplot(aes(conaf,pred)) +
  geom_col() +
  geom_text(data = data_px, aes(x = -Inf, y = .7, label = sum), hjust = -.1) +
  facet_wrap(~MODIS,ncol = 3, scales = 'free_x') +
  scale_y_continuous(limits = c(0,.8), expand = c(0,0)) +
  theme_bw() +
  theme(strip.background = element_rect(fill='white'))




colMeans(conf_matrix_na)
