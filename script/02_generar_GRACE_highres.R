library(fs)
library(terra)
library(rnaturalearth)
library(ncdf4)
library(sf)
library(tidyverse)

# 1. Preparar datos GRACE ----
dir_name <- 'GRC_TELLUS_JPL_GRAC-GRFO_MASCON_CRI_GRID_RL06.1_V3' 

chl <- ne_countries(country='chile',returnclass = 'sf') |> 
  st_transform(4326)

grace_sf <- rast(paste0('/mnt/data_raw/GRACE/',dir_name,'/GRCTellus.JPL.200204_202302.GLO.RL06.1M.MSCNv03CRI.nc'),subds = 'scale_factor')
grace <- rast(paste0('/mnt/data_raw/GRACE/',dir_name,'/GRCTellus.JPL.200204_202302.GLO.RL06.1M.MSCNv03CRI.nc'),subds = 'lwe_thickness')
grace_sf <- resample(grace_sf,grace)

grace <- grace*grace_sf

crs(grace) <- 'EPSG:4326'
grace <- rotate(grace)
grace_chl <- crop(grace,chl)
time(grace)[193:204]
grace_chl <- subset(grace_chl,193:204) 
plot(grace_chl)

# 1. Preparar indicadores TerraClimate
dir <- '/mnt/data_procesada/papers/GRACE-High_res/TerraClimate'

files <- dir_ls(dir,recurse = TRUE,regexp = "2021")
preds_tc <- rast(files) 

names(preds_tc)[49:60] <- c('WS')
names(preds_tc)[61:72] <- c('WS_SM')

grace_chl_hr <- resample(grace_chl,preds_tc,method = 'cubicspline')
plot(grace_chl_hr[[1]])

# 2. Preparar predictores indicadores de sequía

dir <- '/mnt/data_procesada/papers/GRACE-High_res/'

files_spi <- dir_ls(dir,recurse = TRUE,regexp = '(SPI).*2021-')
files_spei <- dir_ls(dir,recurse = TRUE,regexp = '(SPEI).*2021-')
files_eddi <- dir_ls(dir,recurse = TRUE,regexp = '(EDDI).*2021-')
files_ssi <- dir_ls(dir,recurse = TRUE,regexp = '(SSI).*2021-')

preds_spi <- rast(files_spi) |> resample(preds_tc)
preds_spei <- rast(files_spei) |> resample(preds_tc)
preds_eddi <- rast(files_eddi) |> resample(preds_tc)
preds_ssi <- rast(files_ssi) |> resample(preds_tc)

# 3. Preparar DEM

dem <- geodata::elevation_30s('chile',path = tempdir())
preds_dem <- resample(dem, preds_tc,method = 'cubicspline')

# 4. Predictores de vegetación
dir <- '/mnt/data_procesada/data/rasters/Procesados/MODIS/NDVI.MOD13A3.061/'

files <- dir_ls(dir,regexp = 'chl_2021')

preds_ndvi <- rast(files) |> 
  project(crs(preds_tc)) |> 
  resample(preds_tc,method = 'cubicspline')

#3. Generar tabla con datos

#generar grid para grace
grid <- grace_chl[[1]]
values(grid) <- 1:ncell(grid)
grid_pol <- as.polygons(grid)

sample_pts <- st_centroid(grid_pol |> st_as_sf()) 
#sample_pts <- sample_pts[sample(1:ncell(grid),500),] 
#sample_pts <- st_sample(grid_pol |> st_as_sf(),500)

map(1:12,\(i){
  
  out_preds <- c(grace_chl_hr[[i]],preds_spi[[i]],preds_spei[[i]],preds_eddi[[i]],preds_ssi[[i]],preds_dem,preds_ndvi[[i]],
                 preds_tc[[seq(i,72,12)]])
  
  names(out_preds)[1]  <- 'lwe'
  data_modelo <- terra::extract(out_preds,vect(sample_pts)) |> 
    select(-ID) |> 
    drop_na()
  
  # 3. Probar modelo RF
  library(tidymodels)
  
  set.seed(456)
  
  splits <- initial_split(data_modelo)
  
  data_train <- training(splits)
  data_test <- testing(splits)
  
  rf_spec <-rand_forest(
    trees = 1000,
    mtry = tune(),
    min_n = tune()) |> 
    set_engine("ranger",importance = 'impurity') |> 
    set_mode("regression")
  
  model_rec <- recipe(lwe~.,data = data_train ) |>
    step_impute_knn(all_numeric_predictors()) |> 
    step_normalize(all_numeric_predictors()) |> 
    step_zv(all_numeric_predictors())
  
  #4. Resampling y tunning
  
  library(stacks)
  ctrl <- control_stack_grid()
  
  set.seed(453)
  vb_folds <- vfold_cv(data_train)
  
  library(bonsai)
  
  grace_res <- 
    workflow_set(
      preproc = list(rec1 = model_rec), 
      models = list(
        RF = rf_spec
      )
    ) |>  
    workflow_map(
      verbose = TRUE,
      seed = 1603,
      resamples = vb_folds,
      grid = 10,
      metrics = metric_set(rsq,rmse, mae),
      control = ctrl
    )
  
  #autoplot(grace_res,select_best = TRUE)
  collect_metrics(grace_res)
  
  model_last_fit <- grace_res |> 
    extract_workflow("rec1_RF") |>  
    finalize_workflow(
      grace_res |>  
        extract_workflow_set_result("rec1_RF") |>  
        select_best(metric = "rsq")
    ) |>  
    last_fit(split = splits, metrics = metric_set(rsq,rmse,mae)) 
  
  out_preds[is.na(out_preds)] <- -9999
  grace_preds <- predict(model_last_fit |> extract_workflow(),as.data.frame(out_preds))
  
  out <- out_preds[[1]]
  
  values(out) <- grace_preds$.pred
  out <- mask(out,chl)
  plot(out)
  writeRaster(out,glue::glue('~/Descargas/GRACE_highres_{month.abb[i]}.tif'),overwrite = TRUE)
})

library(DALEX)
library(DALEXtra)

explainer_rf <- explain_tidymodels(
  model_last_fit |> extract_workflow(),
  data = data_train,
  y = data_train$mean
  
)

vip_rf <- model_parts(explainer_rf, loss_function = loss_root_mean_square)
plot(vip_rf)