library(tidyverse)
library(tidyterra)
library(terra)
library(parallel)

an <- \(x) {as.numeric(x)}
terraOptions(threads = detectCores()-2)

v <- grep('forestal',list.files('data/raw/vectorial',full.names=T,pattern = '.shp'),value=T)[3] |> 
  vect() |> 
  mutate(CLASS = an(ID_USO)*1000 + an(ID_SUBUSO)*100 + an(ID_ESTRUC)*10 + an(ID_COBER)) |> 
  select(CLASS,USO,SUBUSO,ESTRUCTURA,COBERTURA) |> 
  aggregate(by='CLASS',cores = 10)

v |> values() |> View()

cuenca <- vect('data/processed/vectorial/sitio/cuenca.shp')

v |> 
  filter(between(CLASS,3000,4999)) |> 
  crop(cuenca) |> 
  writeVector('data/processed/vectorial/cobertura/forestal.shp',
            overwrite=T)

mask(v_forestal,cuenca)



names(v)

v_edit <- v |> 
  select(names(v)[c(1:3,11:13,44,49,51,53)])

v_edit |> 
  filter(str_detect(USO_ACTUAL, regex('pradera|bosque',ignore_case = T)),
         !str_detect(USO_ACTUAL, regex('plantacion',ignore_case = T)),
         str_detect(USO_ACTUAL, regex('matorral|nativo',ignore_case = T))) |>
  # values() |> View()
  pull(USO_ACTUAL) |> 
  unique() |> 
  sort()

v_edit |> values() |> 
  View()
