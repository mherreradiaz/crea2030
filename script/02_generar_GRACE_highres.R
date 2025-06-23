library(fs)
library(terra)
library(rnaturalearth)
library(ncdf4)
library(sf)
library(tidyverse)
library(glue)

exportRast <- \(r, output.dir, band.name, names = NULL) {
  require(terra)
  require(glue)
  
  if (!dir.exists(output.dir)) dir.create(output.dir, recursive = TRUE)
  
  if (!grepl('/$', output.dir)) output.dir <- glue('{output.dir}/')
  
  if (!is.null(names)) {
    if (length(names) != nlyr(r)) stop("El largo de 'names' no coincide con el número de layers.")
    names(r) <- names
  }
  
  file_names <- glue('{output.dir}{band.name}{names(r)}.tif')
  
  preview <- head(file_names, 10)
  cat('Las capas se guardarán de la siguiente forma (10 primeras):\n')
  cat(paste0(preview, collapse = '\n'), '\n')
  
  respuesta <- readline(prompt = '¿Desea proceder con la exportación? [s/n]: ')
  if (tolower(respuesta) != 's') {
    cat('Exportación cancelada.\n')
    return(invisible(NULL))
  }
  
  lapply(1:nlyr(r), \(i) {
    ly <- r[[i]]
    writeRaster(ly, file_names[i], overwrite = TRUE)
  })
  
  cat('Exportación finalizada.\n')
}
sortRast <- \(r,desc = F) {
  nm = names(r)
  if (desc == F) or <- order(nm) else or <- order(nm, decreasing = T)
  r[[or]]
}

chl <- ne_countries(country='chile',returnclass = 'sv') |> 
  project('EPSG:4326')

# Preparar datos GRACE

grace_stack <- rast('data/raw/raster/GRACE_input/GRACE/GRACE_LWE_2000_2024.tif')

fechas <- gsub('_','-',str_extract(names(grace_stack), '\\d{4}_\\d{2}'))

grace_chl <- grace_stack |>
  crop(chl)

grace_input <- grace_chl |> 
  setNames(fechas) |> 
  subset(paste0(fechas,'-01') < '2024-01-01')

fechas_grace <- names(grace_input)

# Preparar indicadores TerraClimate

dir <- 'data/raw/raster/GRACE_input/TerraClimate/'
var_names <- c('AET','PPT','Q','SOIL','WS','WS_SM')

tc_preds <- lapply(var_names, \(var) {
  files <- dir_ls(dir,recurse = TRUE,regexp = glue('{var}_'), type = 'file')
  fechas <- str_extract(string = files,pattern = glue('(?<={var}_).*?(?=\\.tif)'))
  files |>
    rast() |> 
    setNames(fechas) |> 
    subset(fechas_grace) |> 
    crop(chl)
}) |> 
  setNames(var_names)

grace_input <- resample(grace_input,tc_preds[[1]],method = 'cubicspline')
plot(grace_input[[1]])

# Preparar predictores indicadores de sequía

dir <- 'data/raw/raster/GRACE_input/DroughtIndex/'
index_names <- c('SPI','SPEI','EDDI','SSI')

drought_pred <- lapply(index_names, \(index) {
  files <- dir_ls(dir,recurse = TRUE,regexp = index, type = 'file')
  fechas <- str_extract(string = files,pattern = '(?<=chile_).*?(?=-01\\.tif)')
  files |>
    rast() |> 
    setNames(fechas) |> 
    subset(fechas_grace) |> 
    resample(tc_preds[[1]],method = 'cubicspline') |> 
    crop(chl)
}) |> 
  setNames(index_names)

# Preparar DEM

dem_pred <- rast('data/raw/raster/GRACE_input/DEM/CHL_elv_msk.tif') |> 
  setNames('DEM') |> 
  resample(tc_preds[[1]],method = 'cubicspline')

# Preparar predictores indicadores de vegetacion

files <- dir_ls('data/raw/raster/GRACE_input/NDVI.MOD13A3.061/')

fechas <- str_extract(files, '(?<=chl_)\\d{7}') |> 
  as.Date(format = '%Y%j') |>
  substr(1,7)

ndvi_pred <- rast(files) |> 
  setNames(fechas) |> 
  subset(fechas_grace) |>
  project(crs(tc_preds[[1]])) |> 
  resample(tc_preds[[1]],method = 'cubicspline') |> 
  crop(chl)

ndvi_pred <- ndvi_pred*0.00000001

# Guardar predictores

writeRaster(grace_input,'data/processed/raster/GRACE_input/GRACE.tif')
lapply(c('AET','PPT','Q','SOIL','WS','WS_SM'), \(var) 
       writeRaster(tc_preds[[var]],glue('data/processed/raster/GRACE_input/{var}.tif')))
lapply(c('SPI','SPEI','EDDI','SSI'), \(var) 
       writeRaster(drought_pred[[var]],glue('data/processed/raster/GRACE_input/{var}.tif')))
writeRaster(dem_pred,'data/processed/raster/GRACE_input/DEM.tif',
            overwrite = T)
writeRaster(ndvi_pred,'data/processed/raster/GRACE_input/NDVI.tif',
            overwrite = T)

#3. Generar tabla con datos

#generar grid para grace
grid <- grace_chl[[1]]
values(grid) <- 1:ncell(grid)
grid_pol <- as.polygons(grid)

sample_pts <- st_centroid(grid_pol |> st_as_sf()) 
#sample_pts <- sample_pts[sample(1:ncell(grid),500),] 
#sample_pts <- st_sample(grid_pol |> st_as_sf(),500)

files <- dir_ls('data/processed/raster/GRACE_input/')
pred_names <- str_extract(files,'(?<=input/).*?(?=.tif)')

r_preds <- lapply(files,rast) |> 
  setNames(pred_names)

GRACE <- r_preds[['GRACE']]
DEM <- r_preds[['DEM']]
r_preds <- r_preds[!names(r_preds) %in% c('GRACE','DEM')]

fechas <- names(GRACE)

library(purrr)
library(bonsai)
library(stacks)
library(rsample)
library(recipes)
library(tidymodels)

resultados <- map(1:nlyr(GRACE), \(i) {
  
  out_preds <- c(
    setNames(GRACE[[i]], 'GRACE'),
    rast(lapply(r_preds, \(pred) pred[[i]])),
    DEM
  )
  
  names(out_preds)[match('WS_SM', names(out_preds))] <- 'WSsm'
  names(out_preds)[1] <- 'LWE'
  
  # Extraer datos a puntos y filtrar NA
  data_modelo <- terra::extract(out_preds, vect(sample_pts)) |>
    select(-ID) |>
    drop_na() 

  if (nrow(data_modelo) < 10 || length(unique(data_modelo$LWE)) <= 1) {
    return(met = tibble(
      fecha = as.Date(glue('{fechas[i]}-01')),
      rsq = NA,
      rmse = NA,
      mae = NA
    ))
  }
  
  stats_pred <- data_modelo |>
    summarise(across(everything(), list(mean = mean, sd = sd), na.rm = TRUE)) |> 
    pivot_longer(cols = everything(), names_to = c('variable', '.value'), names_sep = '_') |> 
    mutate(fecha = as.Date(glue('{fechas[i]}-01'))) |> 
    relocate(fecha)
  
  # División de datos
  set.seed(456)
  splits <- initial_split(data_modelo)
  data_train <- training(splits)
  data_test <- testing(splits)
  
  # Especificación modelo RF
  rf_spec <- rand_forest(
    trees = 1000,
    mtry = tune(),
    min_n = tune()
  ) |> 
    set_engine('ranger', importance = 'impurity') |> 
    set_mode('regression')
  
  # Recipe de preprocesamiento
  model_rec <- recipe(LWE ~ ., data = data_train) |>
    step_impute_knn(all_numeric_predictors()) |>
    step_normalize(all_numeric_predictors()) |>
    step_zv(all_numeric_predictors())
  
  # Validación cruzada
  set.seed(453)
  vb_folds <- vfold_cv(data_train)
  ctrl <- control_stack_grid()
  
  grace_res <- workflow_set(
    preproc = list(rec1 = model_rec),
    models = list(RF = rf_spec)
  ) |> 
    workflow_map(
      seed = 1603,
      resamples = vb_folds,
      grid = 10,
      metrics = metric_set(rsq, rmse, mae),
      control = ctrl,
    )
  
  # Ajuste final
  model_last_fit <- grace_res |>
    extract_workflow('rec1_RF') |>
    finalize_workflow(
      grace_res |>
        extract_workflow_set_result('rec1_RF') |>
        select_best(metric = 'rsq')
    ) |>
    last_fit(split = splits, metrics = metric_set(rsq, rmse, mae))
  
  # Importancia de variables
  vip <- model_last_fit |>
    extract_workflow() |>
    extract_fit_parsnip() |>
    vip::vi() |> 
    select(variable = Variable, importance = Importance) |> 
    mutate(fecha = as.Date(glue('{fechas[i]}-01')))
  
  # Guardar métricas
  met <- model_last_fit |> collect_metrics() |>
    select(.metric, .estimate) |>
    pivot_wider(names_from = .metric, values_from = .estimate) |>
    mutate(fecha = as.Date(glue('{fechas[i]}-01'))) |>
    relocate(fecha)
  
  # Exportar
  out_preds[is.na(out_preds)] <- -9999
  grace_preds <- predict(model_last_fit |> extract_workflow(), as.data.frame(out_preds))

  out <- out_preds[[1]]
  values(out) <- grace_preds$.pred
  out <- mask(out, chl)

  writeRaster(out, glue('data/processed/raster/GRACE_highres/GRACE_highres_{fechas[i]}.tif'), 
              overwrite = T)
  
  return(list(
    met = met,
    vip = vip,
    stats = stats_pred
  ))
})

resultados_df   <- map_dfr(resultados, 'met')
vip_df          <- map_dfr(resultados, 'vip')
stats_predictor <- map_dfr(resultados, 'stats')

write_rds(resultados_df,'data/processed/rds/GRACE_downscaling_metrics.rds')
write_rds(vip_df,'data/processed/rds/GRACE_downscaling_VIP.rds')
write_rds(stats_predictor,'data/processed/rds/GRACE_downscaling_preds_stats.rds')

# visualizar

data_metrics <- read_rds('data/processed/rds/GRACE_downscaling_metrics.rds')
data_stats <- read_rds('data/processed/rds/GRACE_downscaling_preds_stats.rds')
data_vip <- read_rds('data/processed/rds/GRACE_downscaling_VIP.rds')

# métricas

data_metrics |> 
  reframe(mean_rsq = mean(rsq),
          sd_rsq = sd(rsq),
          mean_rmse = mean(rmse),
          sd_rmse = sd(rmse),
          mean_mae = mean(mae),
          sd_mae = sd(mae),
          trend_rsq = as.numeric(MannKendall(rsq)$tau),
          trend_rmse = as.numeric(MannKendall(rmse)$tau),
          trend_mae = as.numeric(MannKendall(mae)$tau))

data_metrics |>
  rename(RSQ = rsq, RMSE = rmse,MAE = mae) |> 
  filter(fecha > '2002-11-01') |> 
  pivot_longer(cols = c(RSQ, RMSE, MAE), names_to = 'metrica', values_to = 'valor') |>
  ggplot(aes(x = as.Date(fecha), y = valor)) +
  geom_line(na.rm = T,linewidth = .5,alpha = .8) +
  geom_line(stat="smooth",method = "gam",size = 1,alpha = 0.8, color = 'steelblue') +
  facet_wrap(~metrica, scales = 'free_y', ncol = 1,strip.position = 'left') +
  labs(x = NULL,y = NULL) +
  scale_x_date(breaks = seq(as.Date('2004-01-01'), as.Date('2022-01-01'), by = '2 years'),
               minor_breaks = seq(as.Date('2000-01-01'), as.Date('2024-01-01'), by = '1 year'),
               date_labels = '%Y',
               expand = c(.01,0)) +
  theme_bw() +
  theme(strip.background = element_rect(fill='white'))

ggsave('output/fig/GRACE_downcaling/metrics.png',width = 10, height = 6)

data_metrics |> 
  arrange(rsq)

which(fechas == '2022-08')

# stats predictores

data_stats |> 
  na.omit() |> 
  group_by(variable) |> 
  mutate(mean = as.numeric(scale(mean)),
         sd = as.numeric(scale(sd))) |> 
  ggplot(aes(as.Date(fecha),sd,color=variable)) +
  geom_line() +
  theme_bw()

data_stat <- data_stats |> 
  select(fecha,variable,mean,sd) |> 
  group_by(variable) |> 
  mutate(mean = as.numeric(scale(mean)),
         sd = as.numeric(scale(sd)))

data_stat |> 
  na.omit() |> 
  filter(variable == 'LWE') |> 
  ggplot(aes(as.Date(fecha),sd,color=variable)) +
  geom_line() +
  theme_bw()
