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

# ver si id estan en base de users
users <- read_rds("santurban_data/base_usuarios.Rds")

# selecionar las columnas que no son un data frame pra unión con basae central
users %>%
  select(
    users %>% 
      map(., class) %>% 
      unlist() %>% 
      unname() %>% 
      str_detect(., pattern = "data.frame", negate = T) %>% 
      which()
  ) %>% 
  rename(
    author_id = id,
    account_created_at = created_at
  ) %>% 
  left_join(df, .) -> df

# corregir formato de fecha de "account_created_at"
df %>% 
  mutate(
    account_created_at = gsub("T", " ", .$account_created_at) %>% as.POSIXct() - 5*3600,
    fecha = as.Date(account_created_at)
  ) -> df
  

# exportar base general
df %>% 
  write_rds("santurban_data/dataLimpia/dataLimpiaGeneral.rds")


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

