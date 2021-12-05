library(readr)
library(dplyr)
library(tidyr)

# leer los datos y convertir .. en NA
gastos <- read_csv(here::here("datos/Gasto educacion Sudamerica/expenditures_sa.csv"), na = "..")

resultados <- read_csv(here::here("datos/Resultados de aprendizaje Sudamerica/learning_outcomes_sa.csv"), na = "..")

# trasponemos ambas tablas 
gastos_traspuesta <- gastos %>% 
  pivot_longer(cols = `1998 [YR1998]`:`2020 [YR2020]`, 
               names_to = "anio", 
               values_to = "gasto_educacion",
               names_pattern = "(^\\d{4}).+") %>%  # para que quede solo el año
  filter(`Series Code` == "UIS.XGDP.23.FSGOV")

resultados_traspuesta <- resultados %>% 
  pivot_longer(cols = `1998 [YR1998]`:`2020 [YR2020]`, 
               names_to = "anio", 
               values_to = "resultados",
               names_pattern = "(^\\d{4}).+") %>%  # para que quede solo el año
  filter(`Series Code` == "LO.PISA.MAT" |
         `Series Code` == "LO.PISA.SCI" |
         `Series Code` == "LO.PISA.REA")

gastos_resultados = full_join(gastos_traspuesta, resultados_traspuesta,
                 by = c("Country Name", "anio", "Country Code")) %>% 
  filter(!is.na(gasto_educacion)) %>% 
  filter(!is.na(resultados)) %>% 
  select(-c("Series Code.x", "Series.x", "Series.y")) %>% 
  pivot_wider(names_from = `Series Code.y`,
              values_from = resultados) %>% 
  rename("matematica"="LO.PISA.MAT") %>% 
  rename("lectura"="LO.PISA.REA") %>% 
  rename("ciencia"="LO.PISA.SCI")  %>% 
  rowwise()# %>% 
  # mutate(promedio = mean(c(mat, rea, sci)))
