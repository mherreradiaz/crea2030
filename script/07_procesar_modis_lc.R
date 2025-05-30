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

