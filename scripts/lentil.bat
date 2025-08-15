
@echo off
SETLOCAL

echo Welcome to the lentil Installation Guide!

:: Set installation directory to user's home directory
set INSTALL_DIR=%USERPROFILE%\lentilapp

:: 1. Check for Scoop. Install if missing
where scoop >nul 2>nul
IF ERRORLEVEL 1 (
    echo Scoop not detected. Installing Scoop...
    powershell -ExecutionPolicy RemoteSigned -Command "iwr -useb get.scoop.sh | iex"
)
:: Refresh Scoop
call scoop update

:: 2. Check for Erlang. Install if missing
where erl >nul 2>nul
IF ERRORLEVEL 1 (
    echo Erlang not detected. Installing Erlang...
    call scoop install erlang
) ELSE (
    echo Erlang detected.
)

:: 3. Check for Gleam. Install if missing
where gleam >nul 2>nul
IF ERRORLEVEL 1 (
    echo Gleam not detected. Installing Gleam...
    call scoop install gleam
) ELSE (
    echo Gleam detected.
)

:: 4. Clone app repo if not present and build
IF NOT EXIST "%INSTALL_DIR%" (
    git clone https://github.com/Daniel-Shunom/lentil.git "%INSTALL_DIR%"
)
cd "%INSTALL_DIR%"

:: Build the application
gleam build

:: 5. Check for PostgreSQL CLI
where psql >nul 2>nul
IF ERRORLEVEL 1 (
    echo PostgreSQL CLI not found.
    set /p installpg=Install PostgreSQL via Scoop? (y/n): 
    IF /I "%installpg%"=="y" (
        call scoop install postgresql
        echo Please configure PostgreSQL as needed.
    ) ELSE (
        echo Please install and configure PostgreSQL manually, then re-run this script.
        PAUSE
        EXIT /B 1
    )
) ELSE (
    echo PostgreSQL detected.
)

:: 6. Set DATABASE_URL environment variable
set "DATABASE_URL=postgres://user:password@localhost:5432/my_db"
REM Persist for current user
setx DATABASE_URL "postgres://user:password@localhost:5432/my_db" >nul

:: 7. Optional: Automate database schema setup
set DB_USER=user   :: replace with your database user
set DB_NAME=my_db  :: replace with your database name
set SCHEMA_FILE=%USERPROFILE%\path\to\schema.sql  :: update with path relative to home directory

REM Check if PGPASSWORD environment variable is set or prompt user
IF "%PGPASSWORD%"=="" (
    set /p PGPASSWORD=Enter password for user %DB_USER%: 
)

REM Create database if it doesn't exist
powershell -Command ^
"$dbExists = (psql -U %DB_USER% -tc \"SELECT 1 FROM pg_database WHERE datname='%DB_NAME%';\" -w).Trim()" ^
| findstr /C:"1" >nul || (psql -U %DB_USER% -c "CREATE DATABASE %DB_NAME%;" -w)

REM Load schema if file exists
IF EXIST "%SCHEMA_FILE%" (
    psql -U %DB_USER% -d %DB_NAME% -f "%SCHEMA_FILE%" -w
) ELSE (
    echo Schema file %SCHEMA_FILE% not found. Please provide the schema

