required_packages <- c(
  "shiny", "shinydashboard", "quantmod", "tidyverse", "caret", "lubridate",
  "ggplot2", "gridExtra", "forecast", "tseries", "performance", "corrplot",
  "scales", "kableExtra", "DT", "shinycssloaders", "plotly", "moments",
  "TTR", "zoo", "shinyjs"  
)

# Install and load packages
for (package in required_packages) {
  if (!requireNamespace(package, character.only = TRUE, quietly = TRUE)) {
    tryCatch({
      install.packages(package, dependencies = TRUE)
    }, error = function(e) {
      message(sprintf("Error installing package %s: %s", package, e$message))
    })
  }
  library(package, character.only = TRUE)
}

# Source the analysis function
source("stock_analysis.R")

# UI Definition
ui <- dashboardPage(
  skin = "blue",
  
  # Dashboard Header
  dashboardHeader(
    title = "Stock Market Analysis Dashboard",
    titleWidth = 300
  ),
  
  # Dashboard Sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Input Parameters", tabName = "input", icon = icon("sliders")),
      menuItem("Price Analysis", tabName = "price", icon = icon("chart-line")),
      menuItem("Statistical Analysis", tabName = "stats", icon = icon("calculator")),
      menuItem("Technical Indicators", tabName = "technical", icon = icon("chart-area")),
      menuItem("Model Results", tabName = "models", icon = icon("brain")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),  
    tags$head(
      tags$style(HTML("
        .content-wrapper { overflow-y: auto; }
        .box { border-top: 3px solid #3c8dbc; }
        .explanation { 
          background-color: #f4f4f4;
          padding: 15px;
          border-left: 3px solid #3c8dbc;
          margin-bottom: 20px;
        }
        .small-box {margin-bottom: 15px;}
        .metrics-box {
          border-left: 3px solid #3c8dbc;
          padding: 10px;
          margin-bottom: 10px;
          background-color: #fff;
        }
        .shiny-notification {
          position: fixed;
          top: 10%;
          left: 50%;
          transform: translateX(-50%);
        }
        #correlation_plot {
          height: 600px !important;
        }
        .signal-badge {
          padding: 10px 20px;
          border-radius: 5px;
          font-size: 24px;
          font-weight: bold;
          margin: 10px;
        }
        .signal-strong-buy {
          background-color: #28a745;
          color: white;
        }
        .signal-buy {
          background-color: #93c47d;
          color: white;
        }
        .signal-hold {
          background-color: #ffd700;
          color: black;
        }
        .signal-sell {
          background-color: #e06666;
          color: white;
        }
        .signal-strong-sell {
          background-color: #cc0000;
          color: white;
        }
        .confidence-score {
          font-size: 18px;
          margin-top: 10px;
        }
        .error-message {
            color: #721c24;
            background-color: #f8d7da;
            border-color: #f5c6cb;
            padding: 0.75rem 1.25rem;
            margin-bottom: 1rem;
            border: 1px solid transparent;
            border-radius: 0.25rem;
        }
  
        .info-message {
          color: #0c5460;
          background-color: #d1ecf1;
          border-color: #bee5eb;
          padding: 0.75rem 1.25rem;
          margin-bottom: 1rem;
          border: 1px solid transparent;
          border-radius: 0.25rem;
        }
      "))
    ),
    
    tabItems(
      # Input Parameters Tab
      tabItem(
        tabName = "input",
        fluidRow(
          box(
            width = 12,
            title = "Stock Selection",
            status = "primary",
            solidHeader = TRUE,
            column(
              width = 6,
              textInput("stock_symbol", "Enter Stock Symbol:", "AAPL"),
              selectInput("preset_range", "Preset Time Ranges:",
                          choices = c("Last 1 Year" = "1y",
                                      "Last 2 Years" = "2y",
                                      "Last 5 Years" = "5y",
                                      "Custom Range" = "custom")),
              conditionalPanel(
                condition = "input.preset_range == 'custom'",
                dateRangeInput("date_range", "Select Custom Date Range:",
                               start = Sys.Date() - years(5),
                               end = Sys.Date())
              )
            ),
            column(
              width = 6,
              actionButton("analyze", "Analyze Stock", 
                           class = "btn-lg btn-primary",
                           icon = icon("play"),
                           style = "margin-top: 25px;"),
              div(
                class = "explanation",
                style = "margin-top: 20px;",
                HTML("
                  <h4>How to use:</h4>
                  <ol>
                    <li>Enter a valid stock symbol (e.g., AAPL for Apple Inc.)</li>
                    <li>Select a preset time range or choose a custom range</li>
                    <li>Click 'Analyze Stock' to begin analysis</li>
                  </ol>
                  <p><strong>Note:</strong> Analysis may take a few moments depending on the date range selected.</p>
                ")
              )
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Quick Stats",
            status = "primary",
            solidHeader = TRUE,
            div(
              id = "quick-stats",
              class = "row",
              style = "display: none;",
              column(
                width = 4,
                div(class = "metrics-box",
                    h4("Current Price"),
                    textOutput("current_price")
                )
              ),
              column(
                width = 4,
                div(class = "metrics-box",
                    h4("Price Change"),
                    uiOutput("price_change")
                )
              ),
              column(
                width = 4,
                div(class = "metrics-box",
                    h4("Trading Volume"),
                    textOutput("trading_volume")
                )
              )
            )
          )
        )
      ),
      
      # Price Analysis Tab
      tabItem(
        tabName = "price",
        fluidRow(
          box(
            width = 12,
            title = "Price Movement Analysis",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("price_plot", height = "500px") %>% withSpinner(),
            div(
              class = "explanation",
              HTML("
                <strong>Chart Components:</strong>
                <ul>
                  <li><strong>Price Line:</strong> Shows daily closing prices</li>
                  <li><strong>Moving Averages:</strong> 20-day and 50-day trends</li>
                  <li><strong>Bollinger Bands:</strong> Volatility channels (grey area)</li>
                </ul>
                <p>Use the toolbox in the top-right to zoom, pan, and explore specific time periods.</p>
              ")
            )
          )
        ),
        fluidRow(
          box(
            width = 6,
            title = "Returns Distribution",
            status = "primary",
            plotlyOutput("returns_plot") %>% withSpinner(),
            div(
              class = "explanation",
              HTML("
                <strong>Components:</strong>
                <ul>
                  <li><strong>Histogram:</strong> Actual distribution of returns</li>
                  <li><strong>Red Line:</strong> Kernel density estimation</li>
                  <li><strong>Green Line:</strong> Normal distribution reference</li>
                </ul>
              ")
            )
          ),
          box(
            width = 6,
            title = "Volatility Analysis",
            status = "primary",
            plotlyOutput("volatility_plot") %>% withSpinner(),
            div(
              class = "explanation",
              HTML("
                <strong>Components:</strong>
                <ul>
                  <li><strong>Red Line:</strong> Daily volatility</li>
                  <li><strong>Blue Line:</strong> Smoothed trend</li>
                  <li><strong>Shaded Area:</strong> Confidence interval</li>
                </ul>
              ")
            )
          )
        )
      ),
      
      # Statistical Analysis Tab
      tabItem(
        tabName = "stats",
        fluidRow(
          box(
            width = 6,
            title = "Descriptive Statistics",
            status = "primary",
            solidHeader = TRUE,
            DTOutput("summary_stats") %>% withSpinner(),
            div(
              class = "explanation",
              HTML("
                <strong>Key Statistics:</strong>
                <ul>
                  <li><strong>Central Tendency:</strong> Mean, Median</li>
                  <li><strong>Dispersion:</strong> Standard Deviation, Quartiles</li>
                  <li><strong>Shape:</strong> Skewness, Kurtosis</li>
                </ul>
              ")
            )
          ),
          box(
            width = 6,
            title = "Statistical Tests",
            status = "primary",
            solidHeader = TRUE,
            verbatimTextOutput("stat_tests") %>% withSpinner(),
            div(
              class = "explanation",
              HTML("
                <strong>Test Interpretations:</strong>
                <ul>
                  <li><strong>ADF Test:</strong> Tests for stationarity
                    <br>H0: Series is non-stationary
                    <br>p < 0.05: Reject H0, series is stationary</li>
                  <li><strong>Shapiro-Wilk Test:</strong> Tests for normality
                    <br>H0: Returns are normally distributed
                    <br>p < 0.05: Reject H0, returns are not normal</li>
                </ul>
              ")
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Correlation Analysis",
            status = "primary",
            plotOutput("correlation_plot", height = "600px") %>% withSpinner(),
            div(
              class = "explanation",
              HTML("
                <strong>Correlation Matrix Guide:</strong>
                <ul>
                  <li>Blue colors indicate positive correlations</li>
                  <li>Red colors indicate negative correlations</li>
                  <li>Darker colors represent stronger relationships</li>
                  <li>Numbers show exact correlation coefficients</li>
                </ul>
              ")
            )
          )
        )
      ),
      
      # Technical Indicators Tab
      tabItem(
        tabName = "technical",
        fluidRow(
          box(
            width = 12,
            title = "Technical Indicators Overview",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("technical_plot", height = "600px") %>% withSpinner(),
            div(
              class = "explanation",
              HTML("
                <strong>Technical Indicators Explained:</strong>
                <ul>
                  <li><strong>RSI (Relative Strength Index):</strong>
                    <br>Measures momentum and overbought/oversold conditions
                    <br>Above 70: Potentially overbought
                    <br>Below 30: Potentially oversold</li>
                  <li><strong>Moving Averages:</strong>
                    <br>20-day MA: Short-term trend
                    <br>50-day MA: Medium-term trend</li>
                  <li><strong>MACD:</strong> Trend and momentum indicator</li>
                </ul>
              ")
            )
          )
        )
      ),
      
      # Model Results Tab
      tabItem(
        tabName = "models",
        fluidRow(
          box(
            width = 12,
            title = "Price Predictions",
            status = "primary",
            plotlyOutput("prediction_plot", height = "500px") %>% withSpinner(),
            div(
              class = "explanation",
              HTML("
                <strong>Model Comparison:</strong>
                <ul>
                  <li><strong>Linear Regression:</strong> Uses multiple features to predict price</li>
                      <ul>
                          <li>Technical indicators (RSI, Moving Averages)</li>
                          <li>Price data (Open, High, Low, Close)</li>
                          <li>Volume and volatility measures</li>
                     </ul>
                </ul>
              ")
            )
          )
        ),
        fluidRow(
          box(
            width = 6,
            title = "Model Performance Metrics",
            status = "primary",
            DTOutput("model_metrics") %>% withSpinner(),
            div(
              class = "explanation",
              HTML("
                <strong>Metrics Explained:</strong>
                <ul>
                  <li><strong>RMSE:</strong> Root Mean Square Error (lower is better)</li>
                  <li><strong>MAE:</strong> Mean Absolute Error (lower is better)</li>
                  <li><strong>MAPE:</strong> Mean Absolute Percentage Error (lower is better)</li>
                  <li><strong>R²:</strong> Coefficient of determination (higher is better)</li>
                </ul>
              ")
            )
          ),
          box(
            width = 6,
            title = "Model Summary",
            status = "primary",
            verbatimTextOutput("model_summary") %>% withSpinner(),
            div(
              class = "explanation",
              "Detailed statistical summary of the Linear Regression model,
               showing coefficient estimates, standard errors, and significance levels."
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Trading Signals",
            status = "primary",
            solidHeader = TRUE,
            
            fluidRow(
              column(
                width = 4,
                div(
                  class = "metrics-box text-center",
                  style = "padding: 20px;",
                  h3("Current Signal"),
                  uiOutput("signal_badge"),
                  uiOutput("confidence_score")
                )
              ),
              column(
                width = 4,
                div(
                  class = "metrics-box",
                  h4("Price Metrics"),
                  tableOutput("price_metrics")
                )
              ),
              column(
                width = 4,
                div(
                  class = "metrics-box",
                  h4("Technical Indicators"),
                  tableOutput("tech_indicators")
                )
              )
            ),
            
            div(
              class = "explanation",
              HTML("
        <strong>Signal Components:</strong>
        <ul>
          <li>Regression Model: Predicts price movements and expected returns</li>
          <li>RSI: Indicates overbought (>70) or oversold (<30) conditions</li>
          <li>Moving Averages: Trend direction and strength</li>
          <li>Volatility: Risk level and market conditions</li>
        </ul>
        <strong>Signal Interpretation:</strong>
        <ul>
          <li>Strong Buy/Sell: High confidence signal with multiple confirming indicators</li>
          <li>Buy/Sell: Moderate confidence signal with some confirming indicators</li>
          <li>Hold: Either conflicting signals or no strong directional indication</li>
        </ul>
      ")
            )
          )
        ),
      ),
      
      # About Tab
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            width = 12,
            title = "About This Dashboard",
            status = "primary",
            solidHeader = TRUE,
            HTML("
              <div class='explanation'>
                <h4>Stock Market Analysis Dashboard</h4>
                <p>This dashboard provides comprehensive analysis of stock market data including:</p>
                <ul>
                  <li>Price analysis and visualization</li>
                  <li>Statistical tests and descriptive statistics</li>
                  <li>Technical indicators</li>
                  <li>Predictive modeling using Linear Regression</li>
                </ul>
                <hr>
                <h4>Data Source</h4>
                <p>All data is fetched from Yahoo Finance via the quantmod package.</p>
                <hr>
                <h4>Analysis Components</h4>
                <ol>
                  <li><strong>Price Analysis:</strong> Visualizes price movements, returns distribution, and volatility</li>
                  <li><strong>Statistical Analysis:</strong> Provides key statistics and hypothesis tests</li>
                  <li><strong>Technical Analysis:</strong> Shows various technical indicators used by traders</li>
                  <li><strong>Predictive Models:</strong> Implements and compares different forecasting approaches</li>
                </ol>
                <hr>
                <h4>Disclaimer</h4>
                <p>This tool is for educational and research purposes only. It should not be used as the sole basis for investment decisions.</p>
              </div>
            ")
          )
        )
      )
    )
  )
)

# Server Definition
# Update the server section with these fixes

# Server Definition
server <- function(input, output, session) {
  # Reactive values to store analysis results
  results <- reactiveVal(NULL)
  
  # Update date range based on preset selection
  observe({
    if (input$preset_range != "custom") {
      years <- as.numeric(substr(input$preset_range, 1, 1))
      updateDateRangeInput(session, "date_range",
                           start = Sys.Date() - years(years),
                           end = Sys.Date()
      )
    }
  })
  
  # Observe analyze button click
  observeEvent(input$analyze, {
    # Validate input
    if (nchar(input$stock_symbol) == 0) {
      showNotification("Please enter a stock symbol", type = "error")
      return()
    }
    
    # Show progress notification
    withProgress(message = 'Analyzing stock data...', value = 0, {
      
      incProgress(0.2, detail = "Fetching data...")
      # Run analysis
      tryCatch({
        result <- analyze_stock(
          toupper(input$stock_symbol),  # Convert to uppercase
          input$date_range[1],
          input$date_range[2]
        )
        results(result)
        
        incProgress(0.8, detail = "Processing complete")
        showNotification("Analysis complete!", type = "message")
        
        # Show quick stats
        shinyjs::show("quick-stats")
        
      }, error = function(e) {
        showNotification(
          paste("Error:", e$message),
          type = "error",
          duration = NULL
        )
      })
    })
  })
  
  # Quick Stats Outputs
  output$current_price <- renderText({
    req(results())
    latest_price <- tail(results()$data$Close, 1)
    scales::dollar(latest_price)
  })
  
output$price_change <- renderUI({
  req(results())
  prices <- tail(results()$data$Close, 2)
  price_change <- diff(prices)
  price_change_pct <- (price_change / prices[1]) * 100
  
  color <- if (price_change >= 0) "color: green;" else "color: red;"
  
  tagList(
    span(style = color, sprintf("%s (%.2f%%)", scales::dollar(price_change), price_change_pct))
  )
})


  
  output$trading_volume <- renderText({
    req(results())
    latest_volume <- tail(results()$data$Volume, 1)
    scales::comma(latest_volume)
  })
  
  # Price Analysis Tab Outputs
  output$price_plot <- renderPlotly({
    req(results())
    p <- results()$plots$price_plot +
      theme(legend.position = "bottom")
    
    ggplotly(p) %>%
      layout(hoverlabel = list(bgcolor = "white"),
             legend = list(orientation = "h", y = -0.2))
  })
  
  output$returns_plot <- renderPlotly({
    req(results())
    ggplotly(results()$plots$returns_plot) %>%
      layout(hoverlabel = list(bgcolor = "white"))
  })
  
  output$volatility_plot <- renderPlotly({
    req(results())
    ggplotly(results()$plots$volatility_plot) %>%
      layout(hoverlabel = list(bgcolor = "white"))
  })
  
  # Statistical Analysis Tab Outputs
  output$summary_stats <- renderDT({
    req(results())
    stats_df <- results()$summary_stats
    
    datatable(
      stats_df,
      options = list(
        dom = 't',
        ordering = FALSE,
        columnDefs = list(
          list(className = 'dt-center', targets = 1:2)
        )
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = c("Price", "Returns"), digits = 4) %>%
      formatStyle(
        columns = 1:3,
        backgroundColor = 'white',
        borderBottom = '1px solid #ddd'
      )
  })
  
  output$stat_tests <- renderPrint({
    req(results())
    cat("Augmented Dickey-Fuller Test:\n")
    print(results()$adf_test)
    cat("\nShapiro-Wilk Normality Test:\n")
    print(results()$shapiro_test)
  })
  
  output$correlation_plot <- renderPlot({
    req(results())
    # Create correlation matrix
    cor_matrix <- cor(results()$data[, c("Close", "Returns", "Volatility", 
                                         "Volume", "MA20", "RSI", "MACD")])
    
    # Plot correlation matrix
    corrplot(cor_matrix,
             method = "color",
             type = "upper",
             addCoef.col = "black",
             number.cex = 0.7,
             tl.col = "black",
             tl.srt = 45,
             title = "Correlation Matrix of Features",
             mar = c(0,0,1,0))
  }, height = 600)
  
  # Technical Indicators Tab Output
  output$technical_plot <- renderPlotly({
    req(results())
    data <- results()$data
    
    # Create subplot with two y-axes
    subplot(
      # RSI Plot
      plot_ly(data = data) %>%
        add_lines(x = ~Date, y = ~RSI, name = "RSI",
                  line = list(color = 'purple')) %>%
        layout(yaxis = list(title = "RSI",
                            range = c(0, 100),
                            gridcolor = '#eee')) %>%
        add_lines(x = ~Date, y = rep(70, nrow(data)), 
                  line = list(color = 'red', dash = 'dash'), 
                  showlegend = FALSE) %>%
        add_lines(x = ~Date, y = rep(30, nrow(data)), 
                  line = list(color = 'green', dash = 'dash'),
                  showlegend = FALSE),
      
      # Moving Averages and MACD Plot
      plot_ly(data = data) %>%
        add_lines(x = ~Date, y = ~MA20, name = "20-day MA",
                  line = list(color = 'blue')) %>%
        add_lines(x = ~Date, y = ~MA50, name = "50-day MA",
                  line = list(color = 'red')) %>%
        add_lines(x = ~Date, y = ~MACD, name = "MACD",
                  line = list(color = 'green')),
      
      nrows = 2,
      heights = c(0.4, 0.6)
    ) %>%
      layout(
        title = "Technical Indicators",
        showlegend = TRUE,
        legend = list(orientation = 'h'),
        xaxis = list(title = "Date"),
        yaxis2 = list(title = "Price/Indicator Value",
                      gridcolor = '#eee')
      )
  })
  
  # Model Results Tab Outputs
  output$prediction_plot <- renderPlotly({
    req(results())
    ggplotly(results()$plots$prediction_plot) %>%
      layout(hoverlabel = list(bgcolor = "white"),
             legend = list(orientation = "h", y = -0.2))
  })
  
  output$model_metrics <- renderDT({
    req(results())
    metrics_df <- data.frame(
      Metric = c("RMSE", "MAE", "MAPE", "R²"),
      Linear_Regression = results()$metrics$linear_regression
    )
    
    datatable(
      metrics_df,
      options = list(
        dom = 't',
        ordering = FALSE,
        columnDefs = list(
          list(className = 'dt-center', targets = 1)
        )
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = "Linear_Regression", digits = 4) %>%
      formatStyle(
        columns = 1:2,
        backgroundColor = 'white',
        borderBottom = '1px solid #ddd'
      )
  })
  
  output$model_summary <- renderPrint({
    req(results())
    summary(results()$lm_model)
  })
  
  # Trading Signals Outputs
  output$signal_badge <- renderUI({
    req(results())
    tryCatch({
      signal <- results()$trading_signals$summary$current_signal
      
      div(
        class = paste("signal-badge", paste0("signal-", tolower(gsub(" ", "-", signal)))),
        signal
      )
    }, error = function(e) {
      showNotification("Error displaying trading signals", type = "error")
      return(NULL)
    })
  })
  
  output$confidence_score <- renderUI({
    req(results())
    confidence <- results()$trading_signals$summary$confidence
    
    div(
      class = "confidence-score",
      paste("Confidence Score:", confidence, "%")
    )
  })
  
  output$price_metrics <- renderTable({
    req(results())
    metrics <- results()$trading_signals$summary$metrics
    
    data.frame(
      Metric = c("Current Price", "Predicted Price", "Expected Return"),
      Value = c(
        sprintf("$%.2f", metrics$current_price),
        sprintf("$%.2f", metrics$predicted_price),
        sprintf("%.2f%%", metrics$predicted_return)
      )
    )
  }, align = 'l')
  
  output$tech_indicators <- renderTable({
    req(results())
    signals <- results()$trading_signals$summary$supporting_signals
    metrics <- results()$trading_signals$summary$metrics
    
    data.frame(
      Indicator = c("RSI", "Moving Averages", "Volatility"),
      Status = c(
        sprintf("%s (%.1f)", signals$rsi, metrics$rsi_value),
        signals$moving_avg,
        sprintf("%s (%.2f%%)", signals$volatility, metrics$volatility * 100)
      )
    )
  }, align = 'l')
  
  # Add reactive validation
  observe({
    if (!is.null(results())) {
      if (any(is.na(results()$data$Close))) {
        showNotification("Warning: Data contains missing values", type = "warning")
      }
    }
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)