# sobre: análisis de palabras y hasthatgs
library(tidyverse)
library(magrittr)

df <- read_rds("santurban_data/dataLimpia/dataLimpiaGeneral.rds")

# extraer hasthtags en otro data frame
map(df$entities$hashtags, as.data.frame) -> tags

# identficar cuántos usan haashtags y cuáles no
map(tags, ~nrow(.) != 0) %>% unlist() %>% which() -> temp

# armar base de datos por id y fecha 
for(i in temp) {
  tags[[i]] %<>% 
    mutate(
      fecha = df %>% slice(i) %>% pull(fecha), 
      id = df %>% slice(i) %>% pull(id) 
    )
  cat("operando en tweet", i, "\n")
}

# selecionarmlos que tienes hasthatgs
tags[temp] %>% 
  bind_rows() -> tags1

# exprtar base de hashtags
tags1 %>% 
  write_rds("santurban_data/dataLimpia/hasthtags.rds")

