library(tidyverse)
library(terra)
library(glue)

data_gwd <- read_rds('data/processed/rds/GWD_chile.rds') |> 
  filter(year(fecha) == 2021)
v_pozos <- vect('data/processed/vectorial/pozos/pozos_chile.shp')

# pozos_consistentes <- data_gwd |> 
#   na.omit() |> 
#   group_by(cuenca,codigo) |> 
#   reframe(n = n()) |> 
#   filter(n >= 8) |> 
#   pull(codigo)
# 
# v_pozos |> 
#   filter(codigo %in% pozos_consistentes) |> 
#   plot()

grace_files <- list.files('data/raw/raster/GRACE_10km',full.names=T)
fechas <- as.Date(paste0(str_extract(grace_files, "\\d{4}-\\d{2}"),'-01'))

r_grace <- grace_files |> 
  rast() |> 
  setNames(fechas)

data_grace <- extract(r_grace,v_pozos) |> 
  mutate(cuenca = v_pozos$cuenca,
         codigo = v_pozos$codigo,
         .before = ID) |> 
  select(-ID) |> 
  pivot_longer(c(everything(),-cuenca,-codigo), names_to = 'fecha',
               values_to = 'lwe') |> 
  mutate(fecha = as.Date(paste0(fecha,'-01'))) |> 
  select(fecha,cuenca,codigo,everything())

data <- data_grace |> 
  left_join(data_gwd) |> 
  group_by(cuenca,codigo) |> 
  mutate(GWD = as.numeric(scale(GWD))) |> 
  ungroup()

write_rds(data,'data/processed/rds/GWD_GRACE.rds')

# correlaciones

data <- read_rds('data/processed/rds/GWD_GRACE.rds')

cor_general <- data |> # correlación general
  reframe(r = cor(GWD, lwe, method = 'pearson', use = 'complete.obs'))

write_rds(cor_general,'data/processed/rds/cor_GRACE_general.rds')

cor_mes_pozo <- data |> # correlacion temporal a nivel de pozo
  group_by(codigo) |> 
  reframe({
    n <- sum(complete.cases(GWD, lwe))
    if (n > 3) {
      test <- cor.test(GWD, lwe, method = 'pearson', use = 'complete.obs')
      tibble(r = test$estimate,
             p_value = test$p.value,
             n = n)
    } else {
      tibble(r = NA, p_value = NA, n = n)
    }
  }) |> 
  na.omit()

write_rds(cor_mes_pozo,'data/processed/rds/cor_GRACE_mes_pozo.rds')

left_join(v_pozos, y = cor_mes_pozo) |> 
  writeVector('data/processed/vectorial/pozos/cor_GRACE.shp',
              overwrite=T)

cor_mes <- data |> # correlacion de cada mes
  group_by(mes = month(fecha)) |> 
  reframe({
    n <- sum(complete.cases(GWD, lwe))
    if (n > 3) {
      test <- cor.test(GWD, lwe, method = 'pearson', use = 'complete.obs')
      tibble(r = test$estimate,
             p_value = test$p.value,
             n = n)
    } else {
      tibble(r = NA, p_value = NA, n = n)
    }
  }) |> 
  na.omit()

write_rds(cor_mes,'data/processed/rds/cor_GRACE_mes.rds')




