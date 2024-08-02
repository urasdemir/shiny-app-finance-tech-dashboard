library(shiny)
library(quantmod)
library(PerformanceAnalytics)
library(rsconnect)

# Define UI
ui <- fluidPage(
  titlePanel("Stock Analysis Metrics"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Instructions"),
      p("This app calculates technical metrics for tech stocks using real data from Yahoo Finance. To begin, select a stock and specify your preferred date range to browse these metrics."),
      selectInput("stock", "Choose a stock:",
                  choices = c("Google" = "GOOG",
                              "NVIDIA" = "NVDA",
                              "Microsoft" = "MSFT",
                              "Apple" = "AAPL",
                              "Meta" = "META",
                              "Amazon" = "AMZN",
                              "Oracle" = "ORCL",
                              "Tesla" = "TSLA",
                              "Intel" = "INTC",
                              "AMD" = "AMD",
                              "Salesforce" = "CRM",
                              "Adobe" = "ADBE",
                              "Cisco" = "CSCO",
                              "IBM" = "IBM",
                              "Netflix" = "NFLX")),
      selectInput("index", "Choose an index:",
                  choices = c("NASDAQ" = "^IXIC", "S&P 500" = "^GSPC")),
      dateRangeInput("dates", "Date range:",
                     start = Sys.Date() - 365, end = Sys.Date()),
      selectInput("metric", "Choose a metric:",
                  choices = c("Beta" = "beta",
                              "Sharpe Ratio" = "sharpe",
                              "Volatility" = "volatility",
                              "Annualized Returns" = "returns"))
    ),
    
    mainPanel(
      uiOutput("metricDescription"),
      
      plotOutput("metricPlot"),
      
      verbatimTextOutput("metricValue"),
      
      hr(),
      h4("Data Source and Availability"),
      p("The data used in this application is sourced from Yahoo Finance. The availability of data may vary for different stocks and indices, and the data is updated daily. Yahoo Finance provides historical stock prices, which are used to calculate the various financial metrics presented in this app."),
      
      br(),
      p("Shiny app by Uras Demir")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  getStockData <- reactive({
    stock <- input$stock
    getSymbols(stock, src = "yahoo", from = input$dates[1], to = input$dates[2], auto.assign = FALSE)
  })
  
  getIndexData <- reactive({
    index <- input$index
    getSymbols(index, src = "yahoo", from = input$dates[1], to = input$dates[2], auto.assign = FALSE)
  })
  
  output$metricDescription <- renderUI({
    metric <- input$metric
    if (metric == "beta") {
      tagList(
        h3("What is Beta?"),
        p("Beta is a measure of a stock's volatility in relation to the overall market. A beta of 1 indicates that the stock moves with the market. A beta greater than 1 means the stock is more volatile than the market, while a beta less than 1 means it is less volatile."),
        p("Interpretation: A beta value higher than 1 suggests higher risk and potential returns, while a beta value less than 1 indicates lower risk and potential returns.")
      )
    } else if (metric == "sharpe") {
      tagList(
        h3("What is Sharpe Ratio?"),
        p("The Sharpe Ratio is a measure of risk-adjusted return. It indicates how much excess return is received for the extra volatility endured for holding a riskier asset."),
        p("Interpretation: A higher Sharpe Ratio indicates better risk-adjusted performance. A Sharpe Ratio above 1 is considered good, above 2 is very good, and above 3 is excellent.")
      )
    } else if (metric == "volatility") {
      tagList(
        h3("What is Volatility?"),
        p("Volatility is a statistical measure of the dispersion of returns for a given security. It represents the degree of variation of a trading price series over time."),
        p("Interpretation: Higher volatility indicates higher risk, as the security's price can vary significantly over a short period. Lower volatility indicates a more stable security.")
      )
    } else if (metric == "returns") {
      tagList(
        h3("What are Annualized Returns?"),
        p("Annualized Returns represent the geometric average amount of money earned by an investment each year over a given time period."),
        p("Interpretation: Higher annualized returns indicate better performance over time. It helps compare the performance of different investments on an annual basis.")
      )
    }
  })
  
  output$metricPlot <- renderPlot({
    stockData <- getStockData()
    indexData <- getIndexData()
    
    stockReturns <- dailyReturn(Cl(stockData))
    indexReturns <- dailyReturn(Cl(indexData))
    
    metric <- input$metric
    
    if (metric == "beta") {
      chart.RollingRegression(stockReturns, indexReturns, width = 60, 
                              main = paste("Rolling Beta of", input$stock, "vs", input$index), 
                              ylab = "Beta", legend.loc = "topright")
    } else if (metric == "sharpe") {
      chart.RollingPerformance(stockReturns, FUN = "SharpeRatio.annualized", width = 60, 
                               main = paste("Rolling Sharpe Ratio of", input$stock), 
                               ylab = "Sharpe Ratio", legend.loc = "topright")
    } else if (metric == "volatility") {
      chart.RollingPerformance(stockReturns, FUN = "sd.annualized", width = 60, 
                               main = paste("Rolling Volatility of", input$stock), 
                               ylab = "Volatility", legend.loc = "topright")
    } else if (metric == "returns") {
      chart.CumReturns(stockReturns, main = paste("Cumulative Returns of", input$stock), 
                       ylab = "Cumulative Returns", legend.loc = "topright")
    }
  })
  
  output$metricValue <- renderPrint({
    stockData <- getStockData()
    indexData <- getIndexData()
    
    stockReturns <- dailyReturn(Cl(stockData))
    indexReturns <- dailyReturn(Cl(indexData))
    
    metric <- input$metric
    
    if (metric == "beta") {
      beta <- CAPM.beta(stockReturns, indexReturns)
      paste("The beta of", input$stock, "relative to", input$index, "is:", round(beta, 2))
    } else if (metric == "sharpe") {
      sharpe <- SharpeRatio.annualized(stockReturns)
      paste("The Sharpe Ratio of", input$stock, "is:", round(sharpe, 2))
    } else if (metric == "volatility") {
      volatility <- sd.annualized(stockReturns)
      paste("The annualized volatility of", input$stock, "is:", round(volatility, 2))
    } else if (metric == "returns") {
      returns <- Return.annualized(stockReturns)
      paste("The annualized returns of", input$stock, "is:", round(returns, 2))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
