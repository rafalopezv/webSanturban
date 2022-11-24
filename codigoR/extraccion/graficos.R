# sobre graficos para extraccion
library(highcharter)
library(tidyverse)

df <- read_rds("santurban_data/paraGraficos/agregadoTipo.rds")

# cambio de lenguage en fechas
lang <- getOption("highcharter.lang")
lang$months <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                 "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

lang$shortMonths <- c("En", "Feb", "Mar", "Abr", "Mayo", "Jun", 
                      "Jul", "Ag", "Sep", "Oct", "Nov", "Dic")

lang$weekdays <- c("Domingo", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", 
                   "Sábado", "Domingo")

options(highcharter.lang = lang)

df %>% 
  hchart(
    "pie", hcaes(x = tipo_tweet, y = n),
    name = "Tweets por tipo", borderColor = "transparent"
  ) %>% 
  hc_tooltip(borderWidth = 1/20) %>% 
  hc_add_theme(hc_theme_elementary()) %>% 
  hc_title(text = "Número y porcentaje de tweets por tipo") %>% 
  hc_subtitle(text = "Pase el mouse sobre el gráfico para mayor información") %>% 
  hc_tooltip(
    pointFormat = paste(
      "<b>{point.tipo_tweet}</b><br>
      Número de casos: <b>{point.n}</b><br>
      Porcentaje [%]: <b>{point.percent100}%</b><br>"
    ),
    headerFormat = ""
  ) -> g1

# agregado general
df1 <- read_rds("santurban_data/paraGraficos/agregadoTipoDia.rds")

df1 %>% 
  group_by(fecha) %>% 
  summarise(n = sum(n)) %>% 
  hchart(
    "line",
    name = "Número de tweets",
    hcaes(
      x = fecha, y = n, 
    )
  ) %>%
  hc_tooltip(table = T, shared = T, sort = T, outside = T, borderWidth = 0.01, 
             style = list(fontFamily = "Open Sans")) %>%
  hc_xAxis(title = list(text = NULL)) %>%
  hc_yAxis(title = list(text = "Número de tweets")) %>%
  hc_plotOptions(line = list(
    lineWidth = 2,
    connectNulls = F,
    animation = list(
      duration = 3000
    ),
    marker = list(
      enabled = F,
      symbol = "circle",
      radius = 2
    )
  )
  ) %>% 
  hc_chart(zoomType = "xy") %>% 
  hc_add_theme(hc_theme_elementary()) %>% 
  hc_title(text = "Número de tweets por día") -> g2


# streamgraph
df1 %>% 
  hchart(
    "line",
    hcaes(
      x = fecha, y = n, group = tipo_tweet
    )
  ) %>%
  hc_tooltip(table = T, shared = T, sort = T, outside = T, borderWidth = 0.01, 
             style = list(fontFamily = "Open Sans")) %>%
  hc_xAxis(title = list(text = NULL)) %>%
  hc_yAxis(title = list(text = "Fallecidos por millón de habitantes")) %>%
  hc_chart(style = list(fontFamily = "Open Sans")) %>%
  hc_plotOptions(line = list(
    lineWidth = 1.5,
    connectNulls = F,
    animation = list(
      duration = 3000
    ),
    marker = list(
      enabled = F,
      symbol = "circle",
      radius = 2
    )
  )
) %>% 
  hc_chart(zoomType = "xy") %>% 
  hc_add_theme(hc_theme_elementary()) %>% 
  hc_title(text = "Número de tweets por día y tipo") -> g3
  
