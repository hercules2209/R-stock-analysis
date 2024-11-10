# Stock Market Analysis System

A comprehensive stock analysis system built in R that provides technical analysis, statistical modeling, and trading signals through an interactive Shiny dashboard.

## Features

- Real-time stock data fetching from Yahoo Finance
- Technical indicators (RSI, MACD, Moving Averages)
- Statistical analysis and visualization
- Linear regression modeling for price prediction
- Trading signals with confidence scores
- Interactive Shiny dashboard interface

## Installation & Setup

There are two ways to run this application:

### Method 1: Using RStudio (Recommended for R Users)

This is the faster and recommended method if you have RStudio installed.

1. **Prerequisites**
   - Install [R](https://cran.r-project.org/) (version 4.0.0 or higher)
   - Install [RStudio](https://posit.co/download/rstudio-desktop/)

2. **Setup Steps**
   ```bash
   # Clone the repository
   git clone https://github.com/hercules2209/stock-analysis.git
   cd stock-analysis
   ```

3. **Open in RStudio**
   - Open RStudio
   - Go to File -> Open Project
   - Navigate to the cloned directory
   - Open `app.R`

4. **Install Dependencies**
   - Click the "Run App" button in RStudio
   - R will prompt to install required packages
   - Click "Yes" to install all required packages
   - Wait for installation to complete (usually 5-10 minutes)

5. **Run the Application**
   - Once packages are installed, the app will launch automatically
   - The app will open in your default web browser


   **Note**: First-time setup will:
   - Create necessary directories
   - Install all required packages (20+ packages, ~500MB)
   - This process can take 15-30 minutes depending on your internet connection
   - Show progress in the logs directory

### Package Dependencies

The following R packages will be installed:
- shiny, shinydashboard
- quantmod, tidyverse
- caret, lubridate
- ggplot2, gridExtra
- forecast, tseries
- performance, corrplot
- scales, kableExtra
- DT, shinycssloaders
- plotly, moments
- TTR, zoo, shinyjs

## Usage

1. Enter a stock symbol (e.g., "AAPL" for Apple Inc.)
2. Select a date range or use preset ranges
3. Click "Analyze Stock"
4. View various analysis tabs:
   - Price Analysis
   - Statistical Analysis
   - Technical Indicators
   - Model Results
   - Trading Signals

## Troubleshooting

### Common Issues

1. **R Not Found Error**
   ```
   Error: R is not installed or not in PATH
   ```
   - Solution: Install R and add it to system PATH

2. **Package Installation Failures**
   - Check logs in `logs/setup.log`
   - Ensure stable internet connection
   - Try running RStudio as administrator (Windows)

3. **Application Won't Start**
   - Check R version (must be 4.0.0 or higher)
   - Verify all required files exist
   - Check logs for specific errors

### Getting Help

If you encounter issues:
1. Check the logs directory for detailed error messages
2. Ensure all prerequisites are installed correctly
3. Try the RStudio method if command-line setup fails
4. Create an issue on [GitHub](https://github.com/hercules2209/stock-analysis/issues)


## Author

**Harshaditya Sharma**
- Email: harshaditya.sharma2022@gmail.com
- GitHub: [@hercules2209](https://github.com/hercules2209)

## Acknowledgments

- This project uses data from Yahoo Finance
- Built with R and Shiny framework
- Uses various R packages for technical analysis and visualization

## Citation

If you use this software in your research, please cite it as:

```
Sharma, Harshaditya. (2024). Stock Market Analysis System. 
GitHub repository: https://github.com/hercules2209/R-stock-analysis
```
