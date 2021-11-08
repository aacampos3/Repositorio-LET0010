library(tidyverse)
library(tidyr)
library(readr)


# cargamos los datos de gastos en educacion en Chile
gastos <- read_delim("datos/Gastos en educacion/expenditures.csv",
                     na = "..", delim = ",")


# arreglamos los datos
# Primero transponemos la tabla y agregamos la columna code_year
# Segundo filtramos, quitando los datos nulos
# Especificamos el codigo de la variable de interés
# Finalmente, agregamos el año como número
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
  labs(title = "Figura 1. Gasto en educación en Chile entre los años 1993 y 2017",
       subtitle = "Como porcentaje del gasto público total",
       x = NULL, y = "Gasto público en eduación",
       caption = "Elaboración propia a partir de datos de https://datatopics.worldbank.org/education/") +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     limits = c(10, 22)) +
  scale_x_continuous(breaks = seq(1993, 2017, by =3)) +
  geom_vline(xintercept = 2006, color = "gray80",
             linetype = "dashed", lwd = 0.75) +
  geom_text(aes(label = round(gasto_año, 1)), vjust = - 2, size = 3) +
  theme_minimal() +
  geom_label(aes(x = 2003.5, y = 21, label = "2006: Revolución Pingüina"),
             color = "gray51", size = 3)


ggsave("figuras/lineas_gasto-educacion_chile.jpeg", height = 7, width = 10)

# Grafico alternativo
gastos_arreglo %>% 
  ggplot(aes(year, gasto_año)) +
  geom_line(col = "turquoise3", lwd = 1.1, alpha = 0.7) +
  geom_point(col = "turquoise4") + 
  labs(title = "Gasto en educación en Chile entre los años 1993 y 2017",
       subtitle = "Como porcentaje del gasto público total",
       x = NULL, y = "Gasto público en eduación") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 22)) +
  scale_x_continuous(breaks = seq(1993, 2017, by =3)) +
  geom_text(aes(label = round(gasto_año, 1)), vjust = - 2, size = 3) +
  theme_minimal()

ggsave("figuras/lineas_gasto-educacion_chile_alternativo.jpeg", height = 7, width = 10)

# Comentarios:
# No pude colocar las etiquetas sin que toparan con el gráfico, por lo que hice
# las lineas un poco más transparantes para que se puediran notar las etiquetas.
# Además, en el eje de los años está graduado cada tres años, para
# que no se sobracargar de información el eje. Además, omití el nombre del
# eje x, porque, en mi opinión,con el título se entiende que la variable
# corresponde a los años
