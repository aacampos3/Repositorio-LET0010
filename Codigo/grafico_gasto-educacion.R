library(tidyverse)
library(tidyr)
library(readr)

source("codigo/limpiza_gastos-chile.R", local = knitr::knit_global())

gastos_porcentaje <- gastos_arreglo %>% 
  filter(`Series Code` == 'SE.XPD.TOTL.GB.ZS')


# Creamos el gráfico
gastos_porcentaje %>% 
  ggplot(aes(year, gasto_anio)) +
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
  geom_text(aes(label = round(gasto_anio, 1)), vjust = - 2, size = 3) +
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
