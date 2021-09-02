# Define server logic required to draw a histogram
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
server <- function(input, output) {
  ################### INPUT ####################
  select_stock <- eventReactive(input$go, {
    
    stock_name <- input$stock
    twin <- input$true_date
    
    #df_stock <- master_df %>% select(input$stock)
    df_stock <- master_df %>% select(date,input$stock)
    df_stock <- filter(df_stock, df_stock$date >= twin[1]  & df_stock$date <= twin[2])
    df_stock <- df_stock %>% select(input$stock)
    #df_stock<-df_stock %>% filter (date >= input$dateRange[1] & date <= input$dateRange[2])
    #df_stock <- master_df %>% filter(Index == stock_name)
    ## FALTA -> FILTRAR O DF POR DATA!!
    
    return(df_stock)
  })
  
  select_stock_data <- eventReactive(input$go, {
    
    stock_name <- input$stock
    twin <- input$true_date
    
    
    df_stock <- master_df %>% select(date,input$stock)
    df_stock <- filter(df_stock, df_stock$date >= twin[1]  & df_stock$date <= twin[2])
    #df_stock <- df_stock %>% select(input$stock)
    
    
    return(df_stock)
  })
  
  
  output$timedate <- renderUI({
    
    dataCharacter<-master_df$date
    min_time <- min(dataCharacter)
    max_time <- max(dataCharacter)
    dateRangeInput("true_date", "Periodo de analise",
                   end = max_time,
                   start = min_time,
                   min  = min_time,
                   max  = max_time,
                   format = "dd/mm/yy",
                   separator = " - ",
                   language='pt-BR')
  })
  
  
  
  ################ OUTPUT #####################
  
  
  Info_DataTable <- eventReactive(input$go,{
    df <- select_stock()
    teste <- input$stock
    
    minimo<-min(df)
    maximo<-max(df)
    
    classeEscolhida<-table(df)
    desvio<- sd(classeEscolhida)
    mean <- df %>% select(input$stock) %>% colMeans()
    Media <- mean[[1]]
    
    Stock <- input$stock
    #moda<- getmode( round(df,1) )
    df<-(words = unlist(df)) #resolveu o problema arguments imply differing number of rows: 1, 0
    minimo<-min(df)
    maximo<-max(df)
    mediana<-median(df)
    moda<- getmode( round(df,1))
    
    df_tb <-  data.frame(Stock,Media,minimo,maximo,desvio,mediana,moda)
    df_tb <- as.data.frame(t(df_tb))
    
    # tb  <- as_tibble(cbind(nms = names(df_tb), t(df_tb)))
    # tb <- tb %>%
    #     rename('Informacoes' = nms,
    #            'Valores' = V2)
    # 
    return(df_tb)
  })
  
  output$info <- renderDT({
    Info_DataTable() %>%
      as.data.frame() %>% 
      DT::datatable(options=list(
        language=list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
        )
      ))
  })
  
  output$sh <- renderPlot({
    # All the inputs
    nome <- input$stock
    
    df <- select_stock_data()
    dtset <- df %>% select(nome)
    aux1 <- min(dtset)
    aux2 <- max(dtset)
    
    df$date <- ymd(df$date)
    a <- df %>% 
      ggplot(aes(date, df[[nome]] , group=1)) +
      geom_path() +
      ylab(nome) +
      coord_cartesian(ylim = c(aux1, aux2)) +
      theme_bw() +
      scale_x_date(date_labels = "%Y-%m-%d")
    
    a
    
  })
  
  
  
  output$historigrama <- renderPlot({
    df <- select_stock()
    df<-table(df)
    
    # dataset:
    data=data.frame(value=rnorm(df))
    
    # basic histogram
    p <- ggplot(data, aes(x=value)) + 
      geom_histogram()
    
    p
    
  })
  output$boxPlot <- renderPlot({
    df <- select_stock()
    b <- boxplot(df,main='Boxplot')
    b
    
  })
  
  
  ## COMP ABA2
  select_stock_comp <- eventReactive(input$go_comp, {
    
    stock_name <- input$stock_comp
    twin <- input$true_date_comp
    
    df_stock <- master_df %>% select(date,stock_name[1], stock_name[2])
    df_stock <- filter(df_stock, df_stock$date >= twin[1]  & df_stock$date <= twin[2])
    
    
    return(df_stock)
  })
  
  output$timedate_comp <- renderUI({
    
    
    dataCharacter2<-master_df$date
    
    min_time2 <- min(dataCharacter2)
    max_time2 <- max(dataCharacter2)
    
    dateRangeInput("true_date_comp", "Periodo de analise",
                   end = max_time2,
                   start = min_time2,
                   min    = min_time2,
                   max    = max_time2,
                   format = "dd/mm/yy",
                   separator = " - ",
                   language='pt-BR')
  })
  
  
  
  
  
  Info_DataTable_comp <- eventReactive(input$go_comp,{
    df <- select_stock_comp()
    colunas <- input$stock_comp
    
    classe1 <- colunas[1]
    classe2 <- colunas[2]
    
    
    media1 <- mean(df[[classe1]])
    media2 <- mean(df[[classe2]])
    
    minimo1<-min(df[[classe1]])
    maximo1<-max(df[[classe1]])
    minimo2<-min(df[[classe2]])
    maximo2<-max(df[[classe2]])
    
    tableClasse1 <- df[[classe1]]
    tableClasse2 <- df[[classe2]]
    
    
    desvio1<- sd(df[[classe1]])
    desvio2<- sd(df[[classe2]])
    
    mediana1<-median(df[[classe1]])
    mediana2<-median(df[[classe2]])
    
    moda1<- getmode( round(df[[classe1]],1))
    moda2<- getmode( round(df[[classe2]],1))
    
    correla <- as.factor(cor(df[[classe1]],df[[classe2]]))
    
    df_tb <-  data.frame(Stock = colunas, Media = c(media1,media2), Minimo = c(minimo1,minimo2), Maximo = c(maximo1,maximo2), Desvio = c(desvio1, desvio2),
                         Mediana = c(mediana1,mediana2), Moda = c(moda1,moda2), Correlacao = correla)
    df_tb <- as.data.frame(t(df_tb))
    
    
    return(df_tb)
  })
  
  output$info_comp <- renderDT({
    Info_DataTable_comp() %>%
      as.data.frame() %>% 
      DT::datatable(options=list(
        language=list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
        )
      ))
  })
  output$sh_comp <- renderPlot({
    df <- select_stock_comp()
    colunas <- input$stock_comp
    
    classe1 <- colunas[1]
    classe2 <- colunas[2]
    
    df$date <- ymd(df$date)
    a <- df %>% 
      ggplot(aes(date, df[[classe1]], color= classe1 , group=1)) +
      geom_path() +
      ylab(colunas[1]) +
      
      theme_bw() +
      scale_x_date(date_labels = "%Y-%m-%d")+ geom_line(aes(y = df[[classe2]], color = classe2 ))
    
    a
  })
  output$br_comp <- renderPlot({
    df <- select_stock_comp()
    colunas <- input$stock_comp
    
    classe1 <- colunas[1]
    classe2 <- colunas[2]
    
    media1 <- mean(df[[classe1]])
    media2 <- mean(df[[classe2]])
    
    data <- data.frame(
      classe=c(classe1,classe2),
      media=c(media1,media2)
    )
    
    # Barplot
    ggplot(data, aes(x=classe, y=media)) + 
      geom_bar(stat = "identity")
  })
  output$scatter_comp <- renderPlot({
    df <- select_stock_comp()
    colunas <- input$stock_comp
    
    classe1 <- colunas[1]
    classe2 <- colunas[2]
    
    l<-ggplot(df, aes(x=df[[classe1]], color= classe1)) + 
      geom_point(aes(y=df[[classe2]], color= classe2))  + labs(y= classe2, x= classe1)
    l
  })
}