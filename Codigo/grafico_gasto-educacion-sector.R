library(tidyverse)
library(tidyr)
library(readr)
library(gghighlight)

source("codigo/limpiza_gastos-chile.R", local = knitr::knit_global())

# arreglamos los datos
gastos_gdp <- gastos_arreglo %>% 
  filter(`Series Code` == 'UIS.XGDP.2.FSGOV' |
        `Series Code` == 'UIS.XGDP.0.FSGOV' |
        `Series Code` == 'UIS.XGDP.1.FSGOV' |
        `Series Code` == 'UIS.XGDP.23.FSGOV'|
        `Series Code` == 'UIS.XGDP.3.FSGOV' |
        `Series Code` == 'UIS.XGDP.56.FSGOV') %>%
  mutate(nombre = ifelse(
    `Series Code` == 'UIS.XGDP.2.FSGOV', "Secundaria Inferior", ifelse(
    `Series Code` == 'UIS.XGDP.0.FSGOV', "Preescolar", ifelse(
    `Series Code` == 'UIS.XGDP.1.FSGOV', "Primaria", ifelse(
    `Series Code` == 'UIS.XGDP.23.FSGOV', "Secundaria", ifelse(
    `Series Code` == 'UIS.XGDP.3.FSGOV', "Secundaria superior", "Terciaria"
    ))))))


# Creamos el gráfico
gastos_gdp %>% 
  ggplot(aes(year, gasto_anio, color = nombre)) +
  geom_line(lwd = 1.1, show.legend = FALSE) +
  scale_x_continuous(limits = c(1998, NA), breaks = seq(1998, 2017, by = 3)) +
  gghighlight(nombre == 'Secundaria', use_direct_label = FALSE) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                       limits = c(0, NA))+
  labs(title = "Evolución del gasto público en educación secundaria como porcentaje del PIN entre los años 1998 y 2017",
       subtitle =  "En comparación a otros niveles educacionales",
       x = "Año",
       y = "Gasto en educación",
       caption = "Elaboración propia a partir de datos de https://datatopics.worldbank.org/education/")+
  theme_minimal() +
  geom_label(aes(2016, 1.7, label = "Secundaria"), show.legend = FALSE)


ggsave("figuras/lineas_gasto-por-sector-educacion_chile.jpeg", height = 7, width = 10)

