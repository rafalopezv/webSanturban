library(tidyverse)
library(reactable)
library(reactablefmtr)

# sobre: gráficos de hastags
tags <- read_rds("santurban_data/dataLimpia/hasthtags.rds")

tags %>% 
  count(tag) %>% 
  rename(
    Hashtag = tag,
    Frecuencia = n
  ) %>% 
  mutate("Porcentaje [%]" = (prop.table(Frecuencia)*100) %>% round(., 3)) %>% 
  arrange(desc(Frecuencia)) -> temp

temp %>% 
  mutate(cumsum = cumsum(`Porcentaje [%]`)) %>% 
  filter(`Porcentaje [%]` <  61) %>% 
  select(-cumsum) -> aa
  
  
# tabla de gráfica
aa %>% 
  reactable(
    theme = reactableTheme(
      style = list(
        fontFamily = "Open Sans"
      )
    ),
    columns = list(
      "Porcentaje [%]" =  colDef(
        cell = color_tiles(
          ., bias = 0.6, 
          box_shadow = F,
          text_size = 11
        ),
        style = list(
          fontFamily = "Roboto Mono"
        )
      ),
      "Frecuencia" =  colDef(
        cell = data_bars(
          ., text_size = 11, 
          box_shadow = F, 
          fill_color = "#4E79A7"
        ),
        style = list(
          fontFamily = "Roboto Mono"
        )
      )
    )
  ) -> tabla_hashtags 







