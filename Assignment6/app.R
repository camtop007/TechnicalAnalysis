install.packages("shiny")
install.packages("ggplot2")
install.packages("quantmod")
library(shiny)
library(ggplot2)
library(quantmod)
#Checking package version
packageVersion("shiny")
packageVersion("ggplot2")
packageVersion("quantmod")
#Testing package 
"package:shiny" %in% search()
"package:ggplot2" %in% search()
"package:quantmod" %in% search()
#Using the 'quantmod' package to fetch hstorical stock data for a given time period
stock_symbol <- "AAPL" # Replace "AAPL" with your desired stock symbol
start_date <- "2025-01-01" # Replace with your desired start date
end_date <- "2025-10-01" # Replace with your desired end date
stock_data <- getSymbols(stock_symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
#Inspect the result to validate the existence of the dataset
head(stock_data)
tail(stock_data)

#Exception handling 
fetch_stock_data <- function(symbol, start_date, end_date) {
  
  tryCatch(
    
    {
      data <- getSymbols(
        symbol,
        src = "yahoo",
        from = start_date,
        to = end_date,
        auto.assign = FALSE
      )
      
      data <- na.omit(data)
      
      return(data)
    },
    
    error = function(e) {
      message(
        "Unable to fetch data for ",
        symbol,
        ": ",
        e$message
      )
      
      return(NULL)
    }
  )
}
# ==========================================
# BDA400 Assignment 6
# Step 2: Visualizing Stock Data
# ==========================================

library(shiny)
library(ggplot2)
library(quantmod)

# Portfolio symbols
portfolio <- c(
  "AAPL",
  "MSFT",
  "GOOGL",
  "TSLA"
)

# Function to fetch stock data
fetch_stock_data <- function(symbol, start_date, end_date) {
  
  tryCatch(
    {
      stock <- getSymbols(
        symbol,
        src = "yahoo",
        from = start_date,
        to = end_date,
        auto.assign = FALSE
      )
      
      stock <- na.omit(stock)
      
      return(stock)
    },
    
    error = function(e) {
      return(NULL)
    }
  )
}

ui <- fluidPage(
  
  titlePanel("BDA400 Stock Portfolio Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        "stock_symbol",
        "Select Stock:",
        choices = portfolio,
        selected = "AAPL"
      ),
      
      dateRangeInput(
        "date_range",
        "Select Date Range:",
        start = "2025-01-01",
        end = Sys.Date()
      ),
      
      selectInput(
        "time_frame",
        "Select Time Frame:",
        choices = c(
          "Daily",
          "Weekly",
          "Monthly"
        ),
        selected = "Daily"
      ),
      
      selectInput(
        "chart_type",
        "Select Chart Type:",
        choices = c(
          "Line",
          "Area"
        ),
        selected = "Line"
      )
    ),
    
    mainPanel(
      
      h3("Stock Price Visualization"),
      
      plotOutput(
        "stock_chart",
        height = "600px"
      )
    )
  )
)

server <- function(input, output) {
  
  stock_data <- reactive({
    
    data <- fetch_stock_data(
      input$stock_symbol,
      input$date_range[1],
      input$date_range[2]
    )
    
    validate(
      need(
        !is.null(data),
        "Unable to retrieve stock data."
      )
    )
    
    return(data)
  })
  
  
  output$stock_chart <- renderPlot({
    
    data <- stock_data()
    
    # Convert xts data to data frame
    plot_data <- data.frame(
      Date = index(data),
      Close = as.numeric(Cl(data))
    )
    
    # Create base chart
    p <- ggplot(
      plot_data,
      aes(
        x = Date,
        y = Close
      )
    )
    
    # Display selected chart type
    if (input$chart_type == "Line") {
      
      p <- p +
        geom_line()
      
    } else if (input$chart_type == "Area") {
      
      p <- p +
        geom_area(
          alpha = 0.4
        )
    }
    
    p <- p +
      labs(
        title = paste(
          input$stock_symbol,
          "Stock Price"
        ),
        x = "Date",
        y = "Closing Price"
      ) +
      theme_minimal()
    
    print(p)
  })
}

server <- function(input, output) {
  
  stock_data <- reactive({
    
    data <- fetch_stock_data(
      input$stock_symbol,
      input$date_range[1],
      input$date_range[2]
    )
    
    validate(
      need(
        !is.null(data),
        "Unable to retrieve stock data."
      )
    )
    
    return(data)
  })
  
  
  output$stock_chart <- renderPlot({
    
    data <- stock_data()
    
    # Convert xts data to data frame
    plot_data <- data.frame(
      Date = index(data),
      Close = as.numeric(Cl(data))
    )
    
    # Create base chart
    p <- ggplot(
      plot_data,
      aes(
        x = Date,
        y = Close
      )
    )
    
    # Display selected chart type
    if (input$chart_type == "Line") {
      
      p <- p +
        geom_line()
      
    } else if (input$chart_type == "Area") {
      
      p <- p +
        geom_area(
          alpha = 0.4
        )
    }
    
    p <- p +
      labs(
        title = paste(
          input$stock_symbol,
          "Stock Price"
        ),
        x = "Date",
        y = "Closing Price"
      ) +
      theme_minimal()
    
    print(p)
  })
}

shinyApp(
  ui = ui,
  server = server
)

checkboxGroupInput(
  "technical_indicators",
  "Technical Indicators:",
  choices = c(
    "Moving Average",
    "RSI",
    "MACD"
  )
  selected = c("Moving Average")
)

sliderInput(
  "short_ma",
  "Short Moving Average Period:",
  min = 5,
  max = 50,
  value = 20
)

sliderInput(
  "long_ma",
  "Long Moving Average Period:",
  min = 20,
  max = 100,
  value = 50
)
