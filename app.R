setwd('C:/Users/inho/Documents/Estatistica e probabilidade/shiny-project')

source('global.R')
source('ui.R')
source('server.R')


shinyApp(
  ui = ui,
  server = server
)
