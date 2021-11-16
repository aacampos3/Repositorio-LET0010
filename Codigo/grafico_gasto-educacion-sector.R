library(tidyverse)
library(tidyr)
library(readr)
library(gghighlight)


# cargamos los datos de gastos en educacion en Chile
gastos <- read_delim("datos/Gastos en educacion/expenditures.csv",
                     na = "..", delim = ",")


# arreglamos los datos
gastos_porcentaje <- pivot_longer(
  gastos, cols= `1970 [YR1970]`:`2020 [YR2020]`, names_to = "code_year",
  values_to = "gasto_año"
) %>% 
  filter(!is.na(gasto_año)) %>%
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
    )))
    )
  )) %>% 
  mutate(code_year, year = as.numeric(substr(code_year, 1, 5)))


# Creamos el gráfico
gastos_porcentaje %>% 
  ggplot(aes(year, gasto_año, color = nombre)) +
  geom_line(lwd = 1.1) +
  scale_x_continuous(limits = c(1998, NA)) +
  gghighlight(nombre == 'Secundaria') +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                       limits = c(0, NA))+
  labs(title = "Evolución del gasto público en educación entre los años 1998 y 2017",
       subtitle =  "Como porcentaje del PIB",
       x = "Año",
       y = "Gasto en educación")+
  theme_minimal()


ggsave("figuras/lineas_gasto-por-sector-educacion_chile.jpeg", height = 7, width = 10)

gastos_porcentaje %>% 
  ggplot(aes(year, gasto_año, fill = `Series Code`)) +
  geom_col(position = "dodge") +
  scale_x_continuous(limits = c(1998, NA))

