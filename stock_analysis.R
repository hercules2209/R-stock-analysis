# Required packages
required_packages <- c(
  "quantmod", "tidyverse", "caret", "lubridate", "ggplot2", "gridExtra",
  "forecast", "tseries", "performance", "corrplot", "scales", "kableExtra",
  "TTR", "zoo", "moments"
)

# Install and load packages
for (package in required_packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

# Function to generate trading signals
generate_trading_signals <- function(data, predictions) {
  # Calculate additional technical signals
  signals <- data %>%
    mutate(
      # RSI signals
      RSI_Signal = case_when(
        RSI > 70 ~ "Sell",
        RSI < 30 ~ "Buy",
        TRUE ~ "Hold"
      ),
      
      # Moving Average signals
      MA_Signal = case_when(
        Close > MA50 & MA20 > MA50 ~ "Buy",
        Close < MA50 & MA20 < MA50 ~ "Sell",
        TRUE ~ "Hold"
      ),
      
      # Volatility signals
      Vol_Signal = case_when(
        Volatility > mean(Volatility, na.rm = TRUE) + 2*sd(Volatility, na.rm = TRUE) ~ "Hold",
        TRUE ~ "Neutral"
      ),
      
      # Price prediction signals (using regression predictions)
      Pred_Return = (predictions - Close) / Close * 100,
      
      Pred_Signal = case_when(
        Pred_Return > 1.0 ~ "Buy",
        Pred_Return < -1.0 ~ "Sell",
        TRUE ~ "Hold"
      )
    )
  
  # Generate combined signal
  signals <- signals %>%
    mutate(
      # Weight the signals
      Final_Signal = case_when(
        # Strong Buy conditions
        (Pred_Signal == "Buy" & (RSI_Signal == "Buy" | MA_Signal == "Buy") & Vol_Signal != "Hold") ~ "Strong Buy",
        # Strong Sell conditions
        (Pred_Signal == "Sell" & (RSI_Signal == "Sell" | MA_Signal == "Sell") & Vol_Signal != "Hold") ~ "Strong Sell",
        # Buy conditions
        (Pred_Signal == "Buy" & (RSI_Signal != "Sell" & MA_Signal != "Sell")) ~ "Buy",
        # Sell conditions
        (Pred_Signal == "Sell" & (RSI_Signal != "Buy" & MA_Signal != "Buy")) ~ "Sell",
        # Default to Hold
        TRUE ~ "Hold"
      )
    )
  
  # Calculate confidence scores
  signals <- signals %>%
    mutate(
      Confidence_Score = case_when(
        Final_Signal == "Strong Buy" ~ 95,
        Final_Signal == "Strong Sell" ~ 95,
        Final_Signal == "Buy" ~ 75,
        Final_Signal == "Sell" ~ 75,
        Final_Signal == "Hold" ~ 50
      )
    )
  
  # Generate signal summary
  signal_summary <- list(
    current_signal = tail(signals$Final_Signal, 1),
    confidence = tail(signals$Confidence_Score, 1),
    supporting_signals = list(
      regression = tail(signals$Pred_Signal, 1),
      rsi = tail(signals$RSI_Signal, 1),
      moving_avg = tail(signals$MA_Signal, 1),
      volatility = tail(signals$Vol_Signal, 1)
    ),
    metrics = list(
      current_price = tail(signals$Close, 1),
      predicted_price = tail(predictions, 1),
      predicted_return = tail(signals$Pred_Return, 1),
      rsi_value = tail(signals$RSI, 1),
      volatility = tail(signals$Volatility, 1)
    )
  )
  
  return(list(
    signals = signals,
    summary = signal_summary
  ))
}

# Main analysis function
analyze_stock <- function(symbol, start_date, end_date) {
  # 1. Data Collection
  stock_data <- tryCatch({
    getSymbols(symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE) %>%
      as.data.frame() %>%
      rownames_to_column(var = "Date")
  }, error = function(e) {
    stop("Error fetching data: ", e$message)
  })
  
  # Rename columns
  colnames(stock_data) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
  
  # 2. Feature Engineering
  processed_data <- stock_data %>%
    mutate(
      Date = as.Date(Date),
      Returns = (Close - lag(Close)) / lag(Close),
      Log_Returns = log(Close / lag(Close)),
      Volatility = (High - Low) / Low,
      Volume_Change = Volume / lag(Volume),
      MA5 = rollmean(Close, 5, fill = NA, align = "right"),
      MA20 = rollmean(Close, 20, fill = NA, align = "right"),
      MA50 = rollmean(Close, 50, fill = NA, align = "right"),
      RSI = RSI(Close, n = 14),
      MACD = MACD(Close)[, 1],
      Signal = MACD(Close)[, 2],
      BB_up = BBands(Close)[, 1],
      BB_mid = BBands(Close)[, 2],
      BB_low = BBands(Close)[, 3],
      Target = lead(Close, 1)
    ) %>%
    na.omit()
  
  # 3. Create summary statistics
  summary_stats <- data.frame(
    Metric = c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Maximum",
               "Standard Deviation", "Skewness", "Kurtosis"),
    Price = c(summary(processed_data$Close), 
              sd(processed_data$Close),
              moments::skewness(processed_data$Close),
              moments::kurtosis(processed_data$Close)),
    Returns = c(summary(processed_data$Returns),
                sd(processed_data$Returns),
                moments::skewness(processed_data$Returns),
                moments::kurtosis(processed_data$Returns))
  )
  
  # 4. Statistical Tests
  adf_test <- adf.test(processed_data$Close)
  shapiro_test <- shapiro.test(processed_data$Returns)
  
  # 5. Visualization Functions
  create_price_plot <- function(data) {
    ggplot(data, aes(x = Date)) +
      geom_line(aes(y = Close, color = "Price")) +
      geom_line(aes(y = MA20, color = "20-day MA")) +
      geom_line(aes(y = MA50, color = "50-day MA")) +
      geom_ribbon(aes(ymin = BB_low, ymax = BB_up), fill = "grey", alpha = 0.2) +
      scale_y_continuous(labels = scales::dollar_format()) +
      labs(title = paste(symbol, "Stock Price and Technical Indicators"),
           x = "Date", y = "Price",
           color = "Indicator") +
      theme_minimal() +
      theme(legend.position = "bottom")
  }
  
  create_returns_plot <- function(data) {
    ggplot(data, aes(x = Returns)) +
      geom_histogram(aes(y = ..density..), bins = 50, fill = "steelblue", alpha = 0.7) +
      geom_density(color = "red", size = 1) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
      stat_function(fun = dnorm, 
                    args = list(mean = mean(data$Returns), 
                                sd = sd(data$Returns)),
                    color = "darkgreen", size = 1, linetype = "dashed") +
      labs(title = paste(symbol, "Returns Distribution"),
           x = "Daily Returns",
           y = "Density") +
      theme_minimal()
  }
  
  create_volatility_plot <- function(data) {
    ggplot(data, aes(x = Date)) +
      geom_line(aes(y = Volatility), color = "darkred") +
      geom_smooth(aes(y = Volatility), method = "loess", color = "blue", se = TRUE) +
      labs(title = paste(symbol, "Historical Volatility"),
           x = "Date",
           y = "Volatility") +
      theme_minimal()
  }
  
  # 6. Model Building
  split_index <- floor(0.8 * nrow(processed_data))
  train_data <- processed_data[1:split_index, ]
  test_data <- processed_data[(split_index + 1):nrow(processed_data), ]
  
  # Cross-validation settings
  ctrl <- trainControl(
    method = "cv",
    number = 5,
    verboseIter = FALSE
  )
  
  # Linear Regression with cross-validation
  features <- c("Open", "High", "Low", "Volume", "Returns", "Volatility",
                "MA5", "MA20", "MA50", "RSI", "MACD", "Signal")
  
  lm_model <- train(
    x = train_data[, features],
    y = train_data$Target,
    method = "lm",
    trControl = ctrl
  )
  
  # Make predictions
  lm_predictions <- predict(lm_model, newdata = test_data)
  
  # Calculate metrics
  calculate_metrics <- function(actual, predicted) {
    rmse <- sqrt(mean((actual - predicted)^2))
    mae <- mean(abs(actual - predicted))
    mape <- mean(abs((actual - predicted) / actual)) * 100
    r2 <- cor(actual, predicted)^2
    c(RMSE = rmse, MAE = mae, MAPE = mape, R2 = r2)
  }
  
  lm_metrics <- calculate_metrics(test_data$Target, lm_predictions)
  
  # Create prediction plot
  create_prediction_plot <- function(test_data, lm_pred) {
    pred_data <- data.frame(
      Date = test_data$Date,
      Actual = test_data$Close,
      LM_Pred = lm_pred
    )
    
    ggplot(pred_data, aes(x = Date)) +
      geom_line(aes(y = Actual, color = "Actual"), size = 1) +
      geom_line(aes(y = LM_Pred, color = "Linear Regression"), 
                linetype = "dashed", size = 1) +
      scale_color_manual(values = c("Actual" = "black", 
                                    "Linear Regression" = "blue")) +
      scale_y_continuous(labels = scales::dollar_format()) +
      labs(title = paste(symbol, "Price Predictions Comparison"),
           x = "Date", y = "Price",
           color = "Type") +
      theme_minimal() +
      theme(legend.position = "bottom")
  }
  
  # Generate trading signals
  trading_signals <- generate_trading_signals(processed_data, lm_predictions)
  
  # Return results
  list(
    data = processed_data,
    summary_stats = summary_stats,
    adf_test = adf_test,
    shapiro_test = shapiro_test,
    lm_model = lm_model,
    metrics = list(
      linear_regression = lm_metrics
    ),
    plots = list(
      price_plot = create_price_plot(processed_data),
      returns_plot = create_returns_plot(processed_data),
      volatility_plot = create_volatility_plot(processed_data),
      prediction_plot = create_prediction_plot(test_data, lm_predictions)
    ),
    trading_signals = trading_signals
  )
}
