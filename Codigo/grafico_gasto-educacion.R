library(tidyverse)
library(tidyr)
library(readr)


# cargamos los datos de gastos en educacion en Chile
gastos <- read_delim("datos/Gastos en educacion/expenditures.csv", na = "..", delim = ",")


# arreglamos los datos
# Primero transponemos la tabla y agregamos la columna code_year
# Segundo Filtramos, quitando los años que no son nulos
# Especificamos el codigo de la variable de interés
# Finalmente, agregamos el año
gastos_arreglo <- pivot_longer(
  gastos, cols= `1970 [YR1970]`:`2020 [YR2020]`, names_to = "code_year",
  values_to = "gasto_año"
) %>% 
  filter(!is.na(gasto_año)) %>% 
  filter(`Series Code` == 'SE.XPD.TOTL.GB.ZS') %>% 
  mutate(code_year, year = as.numeric(substr(code_year, 1, 5)))


# Creamos el gráfico
gastos_arreglo %>% 
  ggplot(aes(year, gasto_año)) +
  geom_line(col = "turquoise3", lwd = 1.1, alpha = 0.7) +
  geom_point(col = "turquoise4") + 
  labs(title = "Gasto en educación en Chile entre los años 1993 y 2017",
       subtitle = "Como porcentaje del gasto público total",
       x = NULL, y = "Gasto en eduación como porcentaje") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_x_continuous(breaks = seq(1993, 2017, by =3)) +
  geom_text(aes(label = round(gasto_año, 1)), vjust = - 2, size = 3) +
  theme_minimal()

# Comentarios:
# No pude colocar las etiquetas sin que toparan con el gráfico, por lo que hice
# las lineas de los gráficos un poco más transparantes para que se puediran 
# notar las etiquetas. Además, en el eje de los años está cada tres años, para
# que no se sobracargar de información el ejer. Además, omití el nombre de los
# ejes, porque, en mi opinión, se entiende con el nombre de los ejes.
