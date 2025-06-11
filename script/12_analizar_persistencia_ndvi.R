library(tidyverse)
library(tidyterra)
library(terra)
library(parallel)

terraOptions(threads = detectCores()-2)

# leer LC (palto y bosque/matorral)

fruticola <- rast('data/processed/raster/catastros/fruticola.tif') |> 
  (\(x) ifel(x == 1, x, NA))() |> 
  setNames('CLASS')
forestal <- rast('data/processed/raster/MOD12Q1/LC_2019.tif') |> 
  (\(x) ifel(x %in% c(1,2), x, NA))() |> 
  setNames('CLASS')

plot(forestal, col = c('green','green4'))

# extraer ndvi de clases

ndvi_files <- list.files('data/processed/raster/MOD13Q1',full.names = T)

fechas <- str_extract(ndvi_files, "\\d{4}-\\d{2}-\\d{2}") |> 
  as.Date()

ndvi <- rast(ndvi_files)

palto <- c(fruticola,mask(ndvi,fruticola)) |> 
  values() |> 
  as_tibble() |> 
  filter(!if_all(everything(), is.na)) |> 
  mutate(cover = 'fruticola',
         .before = CLASS) |> 
  group_by(cover,CLASS) |> 
  mutate(id = 1:n(),
         .before = 2) |> 
  pivot_longer(cols = c(everything(),-cover,-CLASS,-id), names_to = 'fecha',values_to = 'NDVI') |> 
  mutate(fecha = as.Date(fecha)) |> 
  ungroup()

bosque <- c(forestal,mask(ndvi,forestal)) |> 
  values() |> 
  as_tibble() |> 
  filter(!if_all(everything(), is.na)) |> 
  mutate(cover = 'forestal',
         .before = CLASS) |> 
  group_by(cover,CLASS) |> 
  mutate(id = 1:n(),
         .before = 2) |> 
  pivot_longer(cols = c(everything(),-cover,-CLASS,-id), names_to = 'fecha',values_to = 'NDVI') |> 
  mutate(fecha = as.Date(fecha)) |> 
  ungroup()

data_ndvi <- bind_rows(palto,bosque) |> 
  mutate(CLASS = case_when(cover == 'fruticola' ~ 'Palto',
                           cover == 'forestal' & CLASS == 1 ~ 'Forest_1',
                           cover == 'forestal' & CLASS == 2 ~ 'Forest_2',
                           .default = as.character(CLASS))) |> 
  select(CLASS,id,fecha,NDVI)

write_rds(data_ndvi,'data/processed/rds/class_pixel_ndvi.rds')

# analizar persistencia de ndvi de cada punto

data <- read_rds('data/processed/rds/class_pixel_ndvi.rds') |> 
  group_by(CLASS,id,año = year(fecha), mes = month(fecha)) |> 
  reframe(NDVI = mean(NDVI,na.rm=T)) |> 
  ungroup() |> 
  mutate(fecha = as.Date(paste0(año,'-',mes,'-01')))

data |> 
  ggplot(aes(fecha,NDVI)) +
  geom_line(aes(group = id), alpha = .05) +
  scale_x_date(
    breaks = seq(as.Date("2000-01-01"), as.Date("2024-01-01"), by = "4 years"),
    minor_breaks = seq(as.Date("2000-01-01"), as.Date("2024-01-01"), by = "1 year"),
    date_labels = "%Y",
    expand = c(.01,0)) +
  labs(x = NULL) +
  facet_wrap(~CLASS,ncol = 1,strip.position = "right") +
  theme_bw() +
  theme(strip.background = element_rect(fill = 'white'))

ggsave('output/fig/ndvi_series/ndvi_serie.png',width = 13, height = 8)  


