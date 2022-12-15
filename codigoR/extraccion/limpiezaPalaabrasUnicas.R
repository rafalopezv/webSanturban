# anlisis de contenidos
library(tidyverse)
library(tidytext)

df <- read_rds("santurban_data/dataLimpia/dataLimpiaGeneral.rds")
hash <- read_rds("santurban_data/dataLimpia/hasthtags.rds")

hash %>% 
  pull(tag) %>% 
  unique() %>% 
  str_to_lower()  -> hash1
  

df %>% 
  filter(tipo_tweet != "Retweets") %>% 
  select(fecha,  author_id, username, text) %>% 
  unnest_tokens(words, text) %>% 
  count(words) %>% 
  filter(
    !words %in% stopwords::stopwords('es'),
    !words %in% hash1
  ) -> temp


c <- c(
  "t.co", "https", "http", "anla_col", "así", "gran", "silvihabib", "solo",
  "hace", "carloscantep", "x", "ser", "años", "día", "xuronaucfh", 
  "pgncol", "d", "glarce", "puede", "w", "cómo", "todas", "las", "www.visitasantander",
  "2", "10"
)

# fitrar palabras sucias
temp %>% 
  filter(!words %in% c) -> temp

# expotar base
temp %>% 
  write_rds("santurban_data/dataLimpia/conteoPalabras.rds")


  

  


