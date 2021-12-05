library(tidyverse)
library(tidyr)
library(readr)

# cargamos los datos de gastos en educacion en Chile
datos <- read_delim(here::here("datos/Resultados de aprendizaje/learning_outcomes.csv"),
                    na = "..", delim = ",")


# arreglamos los datos
# Primero transponemos la tabla y agregamos la columna code_year
# Segundo Filtramos, quitando los años que no son nulos
# Especificamos el codigo de la variable de interés
# Finalmente, agregamos el año
puntajes_arreglo <- pivot_longer(
  datos, cols= `1970 [YR1970]`:`2020 [YR2020]`, names_to = "code_year",
  values_to = "puntaje_anio") %>% 
  mutate(code_year, year = as.numeric(substr(code_year, 1, 5))) %>%
  select(-c(`2025 [YR2025]`:`2100 [YR2100]`)) %>% 
  filter(!is.na(puntaje_anio) ) %>% 
  filter(`Series Code` == "LO.PISA.REA" | `Series Code` == "LO.PISA.MAT" |
           `Series Code` == "LO.PISA.SCI") %>% 
  select(-code_year)

