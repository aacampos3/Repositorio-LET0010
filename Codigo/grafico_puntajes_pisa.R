library(tidyverse)
library(tidyr)
library(readr)


# cargamos los datos de gastos en educacion en Chile
datos <- read_delim("datos/Resultados de aprendizaje/learning_outcomes.csv",
                    na = "..", delim = ",")


# arreglamos los datos
# Primero transponemos la tabla y agregamos la columna code_year
# Segundo Filtramos, quitando los años que no son nulos
# Especificamos el codigo de la variable de interés
# Finalmente, agregamos el año
puntajes_arreglo <- pivot_longer(
  datos, cols= `1970 [YR1970]`:`2020 [YR2020]`, names_to = "code_year",
  values_to = "puntaje_año") %>% 
  mutate(code_year, year = as.numeric(substr(code_year, 1, 5))) %>%
  select(-c(`2025 [YR2025]`:`2100 [YR2100]`)) %>% 
  filter(!is.na(puntaje_año) ) %>% 
  filter(`Series Code` == "LO.PISA.REA" | `Series Code` == "LO.PISA.MAT" |
           `Series Code` == "LO.PISA.SCI")



# Creamos el gráfico
puntajes_arreglo %>% 
  ggplot(aes(year, puntaje_año, color = `Series Code`)) +
  geom_line(lwd = 1.1, alpha = 0.7) +
  geom_point() +
  theme_minimal() +
  labs(title = "Evolución de los puntaje PISA en Chile",
       subtitle = "Entre los años 2000 y 2018",
       x = NULL, y = "Puntaje PISA") +
  scale_color_discrete("Área",labels = c("Matemática", "Lectura", "Ciencia"), ) +
  scale_y_continuous(limits = c(300, 500)) +
  scale_x_continuous(breaks = seq(200, 2018, by = 3))
?scale_color_discrete

ggsave("figuras/lineas_puntaje_pisa.jpeg", height = 7, width = 10)

# Comentarios:
# No pude colocar las etiquetas sin que toparan con el gráfico, por lo que hice
# las lineas de los gráficos un poco más transparantes para que se puediran 
# notar las etiquetas. Además, en el eje de los años está cada tres años, para
# que no se sobracargar de información el ejer. Además, omití el nombre de los
# ejes, porque, en mi opinión, se entiende con el nombre de los ejes.
