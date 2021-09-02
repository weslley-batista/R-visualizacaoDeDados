

header <- dashboardHeader(title = "Projeto de Estatistica")

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Metricas", tabName = "m", icon = icon("chart-line")),
        menuItem('Comparando Acoes', tabName = 'comp', icon = icon('chart-bar'))
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = 'm',
                fluidRow(
                    box(title = 'Selecione suas opcoes', width=12, solidHeader = TRUE, status='warning',
                        selectInput('stock', 'Acao', stock_list, multiple=FALSE),
                        #dateRangeInput("dateRange", label = h4("Date range"),min=as.Date('2013-01-01'), start=as.Date('2013-01-01'), end=as.Date('2017-01-01'),max=as.Date('2017-01-01')),
                        uiOutput("timedate"),
                        actionButton('go', 'Submeter')
                    )
                ),
                fluidRow(
                    box(title = "Informacoes sobre a acao", width = 12, solidHeader = TRUE,
                        DTOutput('info')
                    )
                ),
                fluidRow(
                    box(title = "Serie de Precos", width = 12, solidHeader = TRUE,
                        plotOutput('sh')
                    )
                ),
                fluidRow(
                    box(title = "Dados em historigrama", width = 12, solidHeader = TRUE,
                        plotOutput('historigrama')
                    )
                ),
                fluidRow(
                    box(title = "Dados em boxPlot", width = 12, solidHeader = TRUE,
                        plotOutput('boxPlot')
                    )
                ),
        ),
        tabItem(tabName = 'comp',
                fluidRow(
                    box(title = 'Selecione suas opcoes', width=12, solidHeader = TRUE, status='warning',
                        selectInput('stock_comp', 'Acao', stock_list, multiple=TRUE),
                        #dateRangeInput("dateRange", label = h4("Date range"),min=as.Date('2013-01-01'), start=as.Date('2013-01-01'), end=as.Date('2017-01-01'),max=as.Date('2017-01-01')),
                        uiOutput("timedate_comp"),
                        actionButton('go_comp', 'Submeter')
                    )
                ),
                fluidRow(
                    box(title = "correlacao entre classes", width = 12, solidHeader = TRUE,
                        DTOutput('info_comp')
                    )
                ),
                fluidRow(
                    box(title = "Grafico de linha (duplo)", width = 12, solidHeader = TRUE,
                        plotOutput('sh_comp')
                    )
                ),
                fluidRow(
                    box(title = "Grafico em barras (medias)", width = 12, solidHeader = TRUE,
                        plotOutput('br_comp')
                    )
                ),
                fluidRow(
                    box(title = "grafico scatterPlot", width = 12, solidHeader = TRUE,
                        plotOutput('scatter_comp')
                    )
                )
        )
    )
)

ui <- dashboardPage(
    skin = 'purple',
    header, sidebar, body)
