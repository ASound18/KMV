library(shiny)
library(quantmod)
library(googleVis)


shinyServer(function(input, output) {
  
  
  ### ~~~~~~~~~~~~~~~ ###
  ###  Tab1: Company  ###
  ### ~~~~~~~~~~~~~~~ ###
  
  output$exchange <- renderUI({
    selectInput("exchange", "Please select an exchange:", choices = c("NYSE", "NASDAQ", "AMEX"))
  })
  
  symbols <- reactive({
    withProgress(message = 'Downloading Symbol List...', value = 0.8, {
    stockSymbols(exchange=as.character(input$exchange))
    })
  })
  
  output$company <- renderUI({
    validate(need(length(input$exchange) >= 1, "Loading..."))
    selectInput("company", "Please select a symbol:", choices = symbols()$Symbol)
  })
 
  output$company_Name <- renderText({
    validate(need(length(input$exchange) >= 1, "Loading..."))
    temp <- symbols()[symbols()$Symbol == input$company,"Name"]
  })
  output$company_LastSale <- renderText({
    validate(need(length(input$exchange) >= 1, "Loading..."))
    temp <- symbols()[symbols()$Symbol == input$company,"LastSale"]
  })
  output$company_MarketCap <- renderText({
    validate(need(length(input$exchange) >= 1, "Loading..."))
    temp <- symbols()[symbols()$Symbol == input$company,"MarketCap"]
  })
  output$company_IPOyear <- renderText({
    validate(need(length(input$exchange) >= 1, "Loading..."))
    temp <- symbols()[symbols()$Symbol == input$company,"IPOyear"]
  })
  output$company_Sector <- renderText({
    validate(need(length(input$exchange) >= 1, "Loading..."))
    temp <- symbols()[symbols()$Symbol == input$company,"Sector"]
  })
  output$company_Industry <- renderText({
    validate(need(length(input$exchange) >= 1, "Loading..."))
    temp <- symbols()[symbols()$Symbol == input$company,"Industry"]
  })
 
  
  
  ### ~~~~~~~~~~~~~~~~~~~~~~ ###
  ###  Tab2: Financial Data  ###
  ### ~~~~~~~~~~~~~~~~~~~~~~ ###
  
  ## symbol OHLC price data
  priceData <- reactive({
    withProgress(message = 'Downloading Price Data...', value = 0.8, {
      getSymbols(Symbol=input$company, src="google")
      objectName <- paste0(input$company)
      get(objectName)
    })
  })
  
  ## symbol financial data
  financialData <- reactive({
    withProgress(message = 'Downloading Financial Data...', value = 0.8, {
      getFinancials(Symbol=input$company, src="google")
      objectName <- paste0(input$company, ".f")
      get(objectName)
    })
  })
  
  ## OHLC price data/chart
  output$radioButtons_OHLC <- renderUI({
    radioButtons("radioButtons_OHLC", "View Chart or Data?", choices = c("Chart", "Data"), inline = FALSE)
  })
  
  output$chart_OHLC <- renderPlot({
    if(input$radioButtons_OHLC == "Chart"){
      withProgress(message = 'Rendering Chart...', value = 0.8, {
        chartSeries(priceData())
        addMACD()
        addBBands()
      })
    }
  })
  
  output$table_OHLC <- renderGvis({
    if(input$radioButtons_OHLC == "Data"){
      withProgress(message = 'Rendering Data...', value = 0.8, {
        data <- priceData()
        df <- as.data.frame(data)
        table <- gvisTable(df)
      })
    }
  })
    
  
  
  ## IS
  output$radioButtons_IS <- renderUI({
    radioButtons("radioButtons_IS", "Quarter or Annual?", choices = c("Quarter" = "Q", "Annual" =  "A"), inline = FALSE)
  })
  
  output$table_IS <- renderGvis({
    validate(need(length(input$radioButtons_IS) != 0, "Loading..."))
    if (input$radioButtons_IS == "Q") {
      data <- financialData()$IS$Q
    } else {
      data <- financialData()$IS$A
    }
    df <- as.data.frame(cbind(rownames(data), data))
    colnames(df)[1] <- "Items"
    table <- gvisTable(df)
  })
  
  ## BS
  output$radioButtons_BS <- renderUI({
    radioButtons("radioButtons_BS", "Quarter or Annual?", choices = c("Quarter" = "Q", "Annual" =  "A"), inline = FALSE)
  })
  
  output$table_BS <- renderGvis({
    validate(need(length(input$radioButtons_BS) != 0, "Loading..."))
    if (input$radioButtons_BS == "Q") {
      data <- financialData()$BS$Q
    } else {
      data <- financialData()$BS$A
    }
    df <- as.data.frame(cbind(rownames(data), data))
    colnames(df)[1] <- "Items"
    table <- gvisTable(df)
  })
  
  ## CF
  output$radioButtons_CF <- renderUI({
    radioButtons("radioButtons_CF", "Quarter or Annual?", choices = c("Quarter" = "Q", "Annual" =  "A"), inline = FALSE)
  })
  
  output$table_CF <- renderGvis({
    validate(need(length(input$radioButtons_CF) != 0, "Loading..."))
    if (input$radioButtons_CF == "Q") {
      data <- financialData()$CF$Q
    } else {
      data <- financialData()$CF$A
    }
    df <- as.data.frame(cbind(rownames(data), data))
    colnames(df)[1] <- "Items"
    table <- gvisTable(df)
  })
  
  
  
  ### ~~~~~~~~~~~~~~~~~~~ ###
  ###  Tab3: Methodology  ###
  ### ~~~~~~~~~~~~~~~~~~~ ###

  
})
