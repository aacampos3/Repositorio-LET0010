library(tidyverse)
library(tidyr)
library(readr)

source("codigo/limpieza_puntajes_pisa.R", local = knitr::knit_global())

# Creamos el gráfico
puntajes_arreglo %>% 
  ggplot(aes(year, puntaje_anio, color = `Series Code`)) +
  geom_line(lwd = 1.1, alpha = 0.7) +
  geom_point() +
  theme_minimal() +
  labs(title = "Evolución de los puntaje PISA en Chile",
       subtitle = "Entre los años 2000 y 2018",
       x = NULL, y = "Puntaje PISA",
       caption = "Elaboración propia a partir de datos de https://datatopics.worldbank.org/education/") +
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

