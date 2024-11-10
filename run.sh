#!/bin/bash

# Get the directory where the script is located
SCRIPT_DIR=$(dirname "$(readlink -f "$0")")
cd "${SCRIPT_DIR}"

# Create logs directory if it doesn't exist
mkdir -p "${SCRIPT_DIR}/logs"

# Function for logging
log_message() {
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    echo "[$timestamp] $1"
    echo "[$timestamp] $1" >> "${SCRIPT_DIR}/logs/run_log_$(date '+%Y-%m-%d').txt"
}

# Check if R is installed
if ! command -v R >/dev/null 2>&1; then
    log_message "Error: R is not installed"
    log_message "Please install R from https://cran.r-project.org/"
    exit 1
fi

# Verify required files exist
for file in "setup.R" "app.R" "stock_analysis.R"; do
    if [ ! -f "$file" ]; then
        log_message "Error: $file not found in ${SCRIPT_DIR}"
        exit 1
    fi
done

# Get R version
R_VERSION=$(R --version | head -n1)
log_message "Found ${R_VERSION}"

# Run setup if not complete
if [ ! -f .setup_complete ]; then
    log_message "Running first-time setup..."
    if ! R --quiet --vanilla -e "source('setup.R')"; then
        log_message "Setup failed. Check logs/setup.log for details."
        exit 1
    fi
    log_message "Setup completed successfully."
fi

# Start the Shiny app from the correct directory
log_message "Launching application..."
log_message "Working directory: ${SCRIPT_DIR}"

R --quiet --vanilla -e "tryCatch({
    setwd('${SCRIPT_DIR}')
    shiny::runApp('app.R', launch.browser=TRUE)
}, error = function(e) {
    cat('Error: ', e\$message, '\n')
    quit(status = 1)
})"

if [ $? -ne 0 ]; then
    log_message "Error: Application failed to start. Check the R console for details."
    exit 1
fi
