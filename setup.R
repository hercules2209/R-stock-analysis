#!/usr/bin/env Rscript

# Get the directory where the script is located
# This ensures correct paths regardless of where the script is run from
SCRIPT_DIR <- dirname(normalizePath(sys.frame(1)$ofile))
setwd(SCRIPT_DIR)  # Set working directory to script location

# Create a log function
log_message <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  formatted_msg <- sprintf("[%s] [%s] %s\n", timestamp, level, msg)
  cat(formatted_msg)
  
  # Also write to log file
  log_dir <- file.path(SCRIPT_DIR, "logs")
  if (!dir.exists(log_dir)) dir.create(log_dir)
  log_file <- file.path(log_dir, "setup.log")
  write(formatted_msg, log_file, append = TRUE)
}

# Function to check R version
check_r_version <- function(min_version = "4.0.0") {
  current_version <- getRversion()
  if (current_version < package_version(min_version)) {
    log_message(sprintf(
      "Required R version: %s. Current version: %s",
      min_version, current_version
    ), "ERROR")
    stop("R version requirement not met")
  }
  log_message(sprintf("R version check passed. Current version: %s", current_version))
}

# Function to create required directories
create_directories <- function() {
  dirs <- c("data", "logs", "output")
  for (dir in dirs) {
    dir_path <- file.path(SCRIPT_DIR, dir)
    if (!dir.exists(dir_path)) {
      dir.create(dir_path)
      log_message(sprintf("Created directory: %s", dir_path))
    } else {
      log_message(sprintf("Directory already exists: %s", dir_path))
    }
  }
}

# Function to check and set CRAN mirror
ensure_cran_mirror <- function() {
  if (is.null(getOption("repos")) || getOption("repos")["CRAN"] == "@CRAN@") {
    # Set to cloud mirror for reliability
    options(repos = c(CRAN = "https://cloud.r-project.org"))
    log_message("Set CRAN mirror to cloud.r-project.org")
  }
}

# Function to check and install required packages
install_required_packages <- function() {
  required_packages <- c(
    "shiny",
    "shinydashboard",
    "quantmod",
    "tidyverse",
    "caret",
    "lubridate",
    "ggplot2",
    "gridExtra",
    "forecast",
    "tseries",
    "performance",
    "corrplot",
    "scales",
    "kableExtra",
    "DT",
    "shinycssloaders",
    "plotly",
    "moments",
    "TTR",
    "zoo",
    "shinyjs"
  )
  
  # Check for missing packages
  new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
  
  if (length(new_packages) > 0) {
    log_message(sprintf("Installing missing packages: %s", 
                       paste(new_packages, collapse=", ")))
    
    # Try to install packages with error handling
    for (pkg in new_packages) {
      tryCatch({
        install.packages(pkg, dependencies=TRUE, quiet=TRUE)
        log_message(sprintf("Successfully installed package: %s", pkg))
      }, error = function(e) {
        log_message(sprintf("Failed to install package %s: %s", pkg, e$message), "ERROR")
        stop(sprintf("Package installation failed: %s", pkg))
      })
    }
  } else {
    log_message("All required packages are already installed")
  }
  
  # Verify all packages can be loaded
  log_message("Verifying package loading...")
  for (pkg in required_packages) {
    tryCatch({
      library(pkg, character.only = TRUE)
      log_message(sprintf("Successfully loaded package: %s", pkg))
    }, error = function(e) {
      log_message(sprintf("Failed to load package %s: %s", pkg, e$message), "ERROR")
      stop(sprintf("Package loading failed: %s", pkg))
    })
  }
}

# Function to verify file structure
verify_file_structure <- function() {
  required_files <- c("app.R", "stock_analysis.R")
  for (file in required_files) {
    file_path <- file.path(SCRIPT_DIR, file)
    if (!file.exists(file_path)) {
      log_message(sprintf("Required file missing: %s", file), "ERROR")
      stop(sprintf("Missing required file: %s", file))
    }
    log_message(sprintf("Verified file exists: %s", file))
  }
}

# Main setup function
main <- function() {
  log_message("Starting setup...")
  
  tryCatch({
    check_r_version()
    create_directories()
    ensure_cran_mirror()
    install_required_packages()
    verify_file_structure()
    
    # Create setup completion marker
    setup_marker <- file.path(SCRIPT_DIR, ".setup_complete")
    file.create(setup_marker)
    log_message("Setup completed successfully!")
    
  }, error = function(e) {
    log_message(sprintf("Setup failed: %s", e$message), "ERROR")
    stop("Setup failed. Check logs for details.")
  })
}

# Run main setup function
main()
