# sobre: limpieza de base de datos de usuarios

# de json a data frame: users_
list.files(pattern = "users_", recursive = T) %>% 
  map(., jsonlite::fromJSON) -> user

# verificar si la lista 1 de la lista user tiene la misma estructura
map(user, ~.[[1]]) %>% map(., colnames) %>% map(., sort) %>% unlist() %>% 
  unique()

# unir metadata de la lista 1 de meta
map_dfr(user, ~.[[1]]) %>% 
  distinct() -> metaData

# extraer columnas que están como data frames paraa separar id únicos
metaData %>% 
  mutate(
    followers_count = .$public_metrics$followers_count,
    following_count = .$public_metrics$following_count,
    tweet_count = .$public_metrics$tweet_count,
    listed_count = .$public_metrics$listed_count
  ) %>% 
  select(-public_metrics) -> metaData
  
# separar id unicos de repetidos
metaData %>% 
  group_by(id) %>% 
  arrange(tweet_count) %>% 
  slice(1) %>% 
  ungroup() -> metaData

# exportar base
metaData %>% 
  write_rds("santurban_data/base_usuarios.Rds")

