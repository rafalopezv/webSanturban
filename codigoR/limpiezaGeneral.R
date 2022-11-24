# sobre: lograr bases unificadas
library(tidyverse)

# de json a data frame: data_
 list.files(pattern = "data_", recursive = T) %>% 
   map_dfr(., jsonlite::fromJSON) -> df
 
#-----------------
# limpieza de base 
#-----------------

# fecha y  hora en formto posixct y ajustar a hora colombiana
# añadir fecha sin hora
df %>% 
   mutate(
     created_at = gsub("T", " ", .$created_at) %>% as.POSIXct() - 5*3600,
     fecha = as.Date(created_at)
   ) -> df
   
#-------------------------
# filtrar tweets únicos
#-------------------------
df %>% 
  distinct() -> df

#-------------------------
# extracción de variables 
#-------------------------

# si el tuit es retuiteado, quoted, etc
df %>% 
   mutate(
     tipo_tweet = map(referenced_tweets, 1),
     tipo_tweet = case_when(
       tipo_tweet == "retweeted" ~ "Retweets",
       tipo_tweet == "replied_to" ~ "Respuesta a",
       tipo_tweet == "quoted" ~ "Cita",
       tipo_tweet == "NULL" ~ "Tweet primario",
       T ~ "Respuesta y retweet"
     )
   ) -> df

# expotar datos para graficas historicas de tipo de tweets
df %>%  
  janitor::tabyl(tipo_tweet) %>% 
  mutate(percent100 = percent*100) %>% 
  mutate_if(is.numeric, round, 3) %>% 
  write_rds("santurban_data/paraGraficos/agregadoTipo.rds")

# por fecha
df %>%  
  group_by(fecha, tipo_tweet) %>% 
  summarise(n = n()) %>% 
  mutate(
    percent = prop.table(n),
    percent100 = percent*100
  ) %>% 
  mutate_if(is.numeric, round, 3) %>% view()
  write_rds("santurban_data/paraGraficos/agregadoTipoDia.rds")

# de json a data frame: users_
temp <- list.files(pattern = "users_", recursive = T)
meta <- map(temp, jsonlite::fromJSON)

# verificar si la lista 1 de la lista meta tiene la misma estructura
map(meta, ~.[[1]]) %>% map(., colnames) %>% map(., sort) %>% unlist() %>% 
  unique()

# unir metadata de la lista 1 de meta
map_dfr(meta, ~.[[1]]) -> metaData

# verificar si la lista 2 de la lista meta tiene la misma estructura
(map(meta, ~.[[2]]) %>% map(., colnames) %>% map(., sort) %>% unlist() %>% 
    unique()) %in% colnames(tweets)

map_dfr(meta, ~.[[2]]) -> metaData1 # unir con tweets


# verificar si la lista 3 de la lista meta tiene la misma estructura
contres <- (map(meta, length) %>% unlist == 3) %>% which(.)

map(meta[contres], ~.[[3]]) %>% map(., colnames) %>% map(., sort) %>% unlist() %>% 
  unique

map(meta[contres], ~.[[3]]) %>% map(., colnames) %>% map(., sort) %>% unlist() %>% 
  unique

map_dfr(meta[contres], ~.[[3]]) -> metaData2 # unir con tweets

