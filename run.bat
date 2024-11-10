@echo off
setlocal enabledelayedexpansion

:: Get the directory where the script is located
set "SCRIPT_DIR=%~dp0"
:: Remove trailing backslash from SCRIPT_DIR
set "SCRIPT_DIR=%SCRIPT_DIR:~0,-1%"
cd /d "%SCRIPT_DIR%"

:: Create logs directory if it doesn't exist
if not exist "logs" mkdir logs

:: Function to get current datetime for logging
set "logdate=%date:~-4,4%-%date:~-10,2%-%date:~-7,2%"
set "logtime=%time:~0,2%:%time:~3,2%:%time:~6,2%"
if "%time:~0,1%"==" " set "logtime=0%time:~1,1%:%time:~3,2%:%time:~6,2%"
set "log_file=logs\run_log_%logdate%.txt"

:: Function to log messages
call :log_message "Starting application setup and launch..."

:: Check if R is installed and in PATH
where R >nul 2>nul
if %ERRORLEVEL% NEQ 0 (
    call :log_message "Error: R is not installed or not in PATH"
    call :log_message "Please install R from https://cran.r-project.org/"
    pause
    exit /b 1
)

:: Verify required files exist
for %%F in (setup.R app.R stock_analysis.R) do (
    if not exist "%%F" (
        call :log_message "Error: %%F not found in %SCRIPT_DIR%"
        pause
        exit /b 1
    )
)

:: Get R version
for /f "tokens=*" %%I in ('R --version ^| findstr "R version"') do set R_VERSION=%%I
call :log_message "Found %R_VERSION%"

:: Run setup if not complete
if not exist ".setup_complete" (
    call :log_message "Running first-time setup..."
    R --quiet --vanilla -e "tryCatch(source('setup.R'), error = function(e) {cat('Error: ', e$message, '\n'); quit(status = 1)})"
    if %ERRORLEVEL% NEQ 0 (
        call :log_message "Setup failed. Check logs/setup.log for details."
        pause
        exit /b 1
    )
    call :log_message "Setup completed successfully."
)

:: Start the Shiny app from the correct directory
call :log_message "Launching application..."
call :log_message "Working directory: %SCRIPT_DIR%"

:: Convert Windows path to R path format (replace backslashes with forward slashes)
set "R_PATH=%SCRIPT_DIR:\=/%"

:: Launch R with proper working directory and error handling
R --quiet --vanilla -e "tryCatch({setwd('%R_PATH%'); shiny::runApp('app.R', launch.browser=TRUE)}, error = function(e) {cat('Error: ', e$message, '\n'); quit(status = 1)})"

if %ERRORLEVEL% NEQ 0 (
    call :log_message "Error: Application failed to start. Check the R console for details."
    pause
    exit /b 1
)

pause
exit /b 0

:log_message
echo [%logdate% %logtime%] %~1
echo [%logdate% %logtime%] %~1 >> "%log_file%"
exit /b 0
