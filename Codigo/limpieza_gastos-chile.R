library(tidyverse)
library(tidyr)
library(readr)

# cargamos los datos de gastos en educacion en Chile
gastos <- read_delim(here::here("datos/Gastos en educacion/expenditures.csv"),
                     na = "..", delim = ",")

# arreglamos los datos
# Primero transponemos la tabla y agregamos la columna code_year
# Segundo filtramos, quitando los datos nulos
# Especificamos el codigo de la variable de interés
# Finalmente, agregamos el año como número
gastos_arreglo <- pivot_longer(
  gastos, cols= `1970 [YR1970]`:`2020 [YR2020]`, names_to = "code_year",
  values_to = "gasto_anio") %>% 
  filter(!is.na(gasto_anio)) %>% 
  filter(`Series Code` == 'SE.XPD.TOTL.GB.ZS'|
         `Series Code` == 'UIS.XGDP.2.FSGOV' |
         `Series Code` == 'UIS.XGDP.0.FSGOV' |
         `Series Code` == 'UIS.XGDP.1.FSGOV' |
         `Series Code` == 'UIS.XGDP.23.FSGOV'|
         `Series Code` == 'UIS.XGDP.3.FSGOV' |
         `Series Code` == 'UIS.XGDP.56.FSGOV') %>% 
  mutate(code_year, year = as.numeric(substr(code_year, 1, 5))) %>% 
  select(-code_year)
