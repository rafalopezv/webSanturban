library(highcharter)
library(tidyverse)

df <- read_rds("santurban_data/dataLimpia/conteoPalabras.rds")

df %>% 
  rename(
    Frecuencia = n
  ) %>% 
  mutate(Porcentaje = (prop.table(Frecuencia)*100) %>% round(., 3)) %>% 
  arrange(desc(Porcentaje)) -> palabras_tabla

df %>% 
  arrange(desc(n)) %>% 
  mutate(
    prop = (prop.table(n)*100) %>% round(., 3),
    sumsum = cumsum(prop)
  ) %>% 
  filter(sumsum < 20) %>% 
  hchart("wordcloud", hcaes(name = words, weight = n)) %>% 
  hc_tooltip(borderWidth = 1/20) %>% 
  hc_tooltip(
    pointFormat = paste(
      "Palabra:<b>{point.words}</b><br>
      Número de apariciones: <b>{point.n}</b><br>
      Porcentaje [%]: <b>{point.prop}%</b><br>"
    ),
    headerFormat = ""
  ) -> palabras
  

  
  


