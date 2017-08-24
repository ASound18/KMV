library(shiny)
library(quantmod)
library(googleVis)
library(timeSeries)
library(ggplot2)

### ~~~~~~~~~~~~~~~~ ###
###  Some functions  ###
### ~~~~~~~~~~~~~~~~ ###

# Black-Scholes Option Value
# Call value is returned in values[1], put in values[2]
blackscholes <- function(S, X, rf, T, sigma) {
  values <- c(2)
  
  d1 <- (log(S/X)+(rf+sigma^2/2)*T)/(sigma*sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  
  values[1] <- S*pnorm(d1) - X*exp(-rf*T)*pnorm(d2)
  values[2] <- X*exp(-rf*T) * pnorm(-d2) - S*pnorm(-d1)
  
  values
}

rooteqn <- function(V, S, X, rf, T, sigma) {
  V - blackscholes(S, X, rf, T, sigma)[1]
}


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
  
  output$downloadData_OHLC <- downloadHandler(
    filename = function() {
      paste(input$company, "OHLC_PriceData.csv", sep= "_")
    },
    content = function(file) {
      data <- as.matrix(priceData())
      df <- as.data.frame(cbind(rownames(data), data))
      colnames(df)[1] <- "Date"
      write.csv(df, file)
    }
  )
  
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
        data <- as.matrix(priceData())
        df <- as.data.frame(cbind(rownames(data), data))
        colnames(df)[1] <- "Date"
        table <- gvisTable(df, options=list(page='enable', pageSize = 30))
      })
    }
  })
    
  
  
  ## IS
  output$radioButtons_IS <- renderUI({
    radioButtons("radioButtons_IS", "Quarter or Annual?", choices = c("Quarter" = "Q", "Annual" =  "A"), inline = FALSE)
  })
  
  output$downloadData_IS <- downloadHandler(
    filename = function() {
      paste(input$company, "IncomeStatement.csv", sep= "_")
    },
    content = function(file) {
      if (input$radioButtons_IS == "Q") {
        data <- financialData()$IS$Q
      } else {
        data <- financialData()$IS$A
      }
      write.csv(data, file)
    }
  )
  
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
  
  output$downloadData_BS <- downloadHandler(
    filename = function() {
      paste(input$company, "BalanceSheet.csv", sep= "_")
    },
    content = function(file) {
      if (input$radioButtons_BS == "Q") {
        data <- financialData()$BS$Q
      } else {
        data <- financialData()$BS$A
      }
      write.csv(data, file)
    }
  )
  
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
  
  output$downloadData_CF <- downloadHandler(
    filename = function() {
      paste(input$company, "CashFlowStatement.csv", sep= "_")
    },
    content = function(file) {
      if (input$radioButtons_CF == "Q") {
        data <- financialData()$CF$Q
      } else {
        data <- financialData()$CF$A
      }
      write.csv(data, file)
    }
  )
  
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
  
  total_common_shares_outstanding_approx <- reactive({
    data <- financialData()$BS$Q
    data[nrow(data),1]
  })
  
  stock_price_filterd <- reactive({
    window(priceData()[,4], start="2016-01-01")
  }) 
  
  equity_value <- reactive({
    stock_price_filterd() * total_common_shares_outstanding_approx()
  })
  
  equity_volatility <- reactive({
    as.numeric(sd(returns(equity_value()), na.rm = TRUE))*sqrt(252)
  })
  
  default_point <- reactive({
    data <- financialData()$BS$Q
    df <- as.data.frame(cbind(rownames(data), data))
    out <- data[df[,1] == "Total Current Liabilities",1] +  (data[df[,1] == "Total Liabilities",1] - data[df[,1] == "Total Current Liabilities",1])/2
    if(is.na(out)){out=0}
    out
  })
  
  asset_value_and_volatility <- reactive({
    asset_value = timeSeries(rep(NA, length(equity_value())), time(equity_value()))
    # initial estimate of volatility
    asset_volatility = equity_volatility()
    asset_volatility_old = 0
    index = 1
    # iterative solution for asset values
    while(abs(asset_volatility - asset_volatility_old)/asset_volatility_old > 0.000001) {
      index = index + 1
      for(i in 1:length(equity_value())) {
        temp = uniroot(rooteqn, interval=c(equity_value()[i], 10*equity_value()[i]), V=equity_value()[i], rf=input$rf, sigma=asset_volatility, X=default_point(), T=1)
        asset_value[i] = temp$root
      }
      asset_volatility_old = asset_volatility
      asset_volatility = as.numeric(sd(returns(asset_value)))*sqrt(252)
    }
    list(asset_value = asset_value, asset_volatility = asset_volatility)
  })
  
  DD <- reactive({
    asset_value_temp = asset_value_and_volatility()$asset_value
    asset_value_temp = asset_value_temp[length(asset_value_temp)]
    asset_volatility_temp = asset_value_and_volatility()$asset_volatility
    t = 1
    rf = input$rf
    
    out = (log(asset_value_temp) - log(default_point()) +(rf - asset_volatility_temp^2/2)*t) / (asset_volatility_temp*sqrt(t))
  })
  
  
  ### ~~~ Step1 ~~~ ##
  output$total_common_shares_outstanding_approx <- renderUI({
    HTML("Total Common Shares Outstanding is:", paste("<b>", total_common_shares_outstanding_approx(),"</b>"), "M")
  })
  
  output$default_point <- renderUI({
    HTML("Default Point (assuming constant) is:", paste("<b>", default_point(),"</b>"), "M")
  })
  
  output$equity_volatility <- renderUI({
    HTML("Volatility of Equity is:", paste("<b>", equity_volatility(),"</b>"))
  })
  
  output$rf <- renderUI({
    sliderInput("rf", "Continuously-Compounded Risk-Free Rate", value = 0.03, min = -1, max = 1, step = 0.001, animate = FALSE)
  })
  
  output$equity_vs_asset_plot <- renderPlot({
    asset_value_temp = asset_value_and_volatility()$asset_value
    equity_value_temp = timeSeries(equity_value())
    
    par(mar=c(3,2,2,1), mgp=c(2,1,0))
    plot(asset_value_temp, ylim=range(asset_value_temp, equity_value_temp), ylab="Values", main = "Asset Value vs. Equity Value (M)")
    lines(equity_value_temp, col = 2)
    legend("bottomright", legend=c("Asset value", "Equity value"), lty=c(1,1), col=c(1,2))
  })
  
  output$asset_volatility <- renderUI({
    HTML("Volatility of Asset is:", paste("<b>", asset_value_and_volatility()$asset_volatility,"</b>"))
  })

  
  ### ~~~ Step2 ~~~ ##
  output$DD <- renderUI({
    HTML("Distance to Default (DD) is:", paste("<b>", DD(),"</b>"))
  })

  
  
  ### ~~~ Step3 ~~~ ##
  
  
  
  
  
  ### ~~~~~~~~~~~~~~~ ###
  ###  Tab4: Contact  ###
  ### ~~~~~~~~~~~~~~~ ###
  output$aig_logo <- renderImage({
    list(src = "www/images/AIG_r_rgb.png", alt = "placeholder for AIG logo")
  }, deleteFile = FALSE)

  
})
