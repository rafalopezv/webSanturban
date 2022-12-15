# análisis de quienes
library(tidyverse)
library(reactable)
library(reactablefmtr)
library(htmltools)

df <- read_rds("santurban_data/dataLimpia/dataLimpiaGeneral.rds")

df %>% 
  filter(tipo_tweet == "Tweet primario") -> temp

#----------------------------------------------------
# ver cuantos retweets y likes generan estas cuentas
#----------------------------------------------------
# extraer métricas
temp %>% 
  mutate(
    retweet_count = .$public_metrics$retweet_count, 
    reply_count = .$public_metrics$reply_count, 
    like_count = .$public_metrics$like_count,
    quote_count = .$public_metrics$quote_count
  ) %>% 
  select(-public_metrics) -> temp

# conteo general
temp %>% 
  group_by(author_id, username) %>% 
  summarise(
    "Número de tweets" = n(),
    Retweets = sum(retweet_count), 
    "Réplicas" = sum(reply_count), 
    Likes = sum(like_count),
    Citas = sum(quote_count)
  ) %>% 
  ungroup() %>% 
  mutate(
    "Total interacciones" = Retweets + Likes + Citas + `Réplicas`,
    "Total interacciones [%]" = (prop.table(`Total interacciones`) * 100) %>% round(., 4),
    "Porcentaje de tweets [%]" = (prop.table(`Número de tweets`)*100) %>% round(., 4)
  ) %>% 
  rename(Usuario = username) %>% 
  select(
    Usuario, `Número de tweets`, `Porcentaje de tweets [%]`, 
    `Total interacciones`, `Total interacciones [%]`, everything()
  ) %>% 
  select(-author_id) %>% 
  arrange(desc(`Total interacciones`)) -> temp1

# grafica reactable  
temp1 %>% 
  mutate(cumsum = cumsum(`Total interacciones [%]`)) %>% 
  filter(cumsum <= 51) %>% 
  select(-cumsum) %>% 
  reactable(
    theme = reactableTheme(
      style = list(
        fontFamily = "Open Sans"
      )
    ),
    columns = list(
      "Porcentaje de tweets [%]" =  colDef(
        cell = color_tiles(
          ., bias = 0.6, 
          box_shadow = F,
          text_size = 11
        ),
        style = list(
          fontFamily = "Roboto Mono"
        )
      ),
      "Número de tweets" =  colDef(
        cell = data_bars(
          ., text_size = 11, 
          box_shadow = F, 
          fill_color = "#4E79A7"
        ),
        style = list(
          fontFamily = "Roboto Mono"
        )
      ),
      "Total interacciones [%]" =  colDef(
        cell = color_tiles(
          ., bias = 0.6, 
          box_shadow = F,
          text_size = 11
        ),
        style = list(
          fontFamily = "Roboto Mono"
        )
      ),
      "Total interacciones" =  colDef(
        cell = data_bars(
          ., text_size = 11, 
          box_shadow = F, 
          fill_color = "#F28E2B"
        ),
        style = list(
          fontFamily = "Roboto Mono"
        )
      ),
      Retweets =  colDef(
        cell = data_bars(
          ., text_size = 11, 
          box_shadow = F, 
          fill_color = "#E15759"
        ),
        style = list(
          fontFamily = "Roboto Mono"
        )
      ),
      Usuario = colDef(
        cell = function(value) {
          url <- paste0("https://twitter.com/", value)
          tags$a(href = url, target = "_blank", paste0("@", value))
        },
        width = 150
      ),
      "Réplicas" =  colDef(
        cell = data_bars(
          ., text_size = 11, 
          box_shadow = F, 
          fill_color = "#E15759"
        ),
        style = list(
          fontFamily = "Roboto Mono"
        )
      ),
      Likes =  colDef(
        cell = data_bars(
          ., text_size = 11, 
          box_shadow = F, 
          fill_color = "#E15759"
        ),
        style = list(
          fontFamily = "Roboto Mono"
        )
      ),
      Citas =  colDef(
        cell = data_bars(
          ., text_size = 11, 
          box_shadow = F, 
          fill_color = "#E15759"
        ),
        style = list(
          fontFamily = "Roboto Mono"
        )
      )
    )
  ) -> tabla_quien 


