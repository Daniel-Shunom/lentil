echo "Welcome to the lentil Installation Guide!"


@echo off
SETLOCAL

:: 1. Check for Scoop. Install if missing
where scoop >nul 2>nul
IF ERRORLEVEL 1 (
    echo Scoop not detected. Installing Scoop...
    powershell -ExecutionPolicy RemoteSigned -Command "iwr -useb get.scoop.sh | iex"
)

:: Refresh Scoop
call scoop update

:: 2. Check for Erlang. Install with Scoop if missing.
where erl >nul 2>nul
IF ERRORLEVEL 1 (
    echo Erlang not detected. Installing Erlang...
    call scoop install erlang
) ELSE (
    echo Erlang detected.
)

:: 3. Check for Gleam. Install with Scoop if missing.
where gleam >nul 2>nul
IF ERRORLEVEL 1 (
    echo Gleam not detected. Installing Gleam...
    call scoop install gleam
) ELSE (
    echo Gleam detected.
)

:: 4. Set up new Gleam project if not present
IF NOT EXIST my_gleam_app (
    gleam new my_gleam_app
)

cd my_gleam_app

:: Example: Add envoy as a dependency (uncomment and add more as needed)
:: gleam add envoy

gleam build

:: 5. Check for PostgreSQL
where psql >nul 2>nul
IF ERRORLEVEL 1 (
    echo PostgreSQL CLI not found.
    set /p installpg="Install PostgreSQL via Scoop? (y/n): "
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

:: 6. Set DATABASE_URL environment variable (set your actual database details)
set "DATABASE_URL=postgres://user:password@localhost:5432/my_db"

:: Optionally persist it for this user session (Windows sets env vars like this)
setx DATABASE_URL "postgres://user:password@localhost:5432/my_db"

:: 7. Instructions for using the environment variable in Gleam
echo(
echo ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
echo In your Gleam code, use envoy:
echo(
echo import envoy
echo(
echo pub fn main() {
echo.    let db_url = envoy.get("DATABASE_URL")
echo.    // use db_url in your app
echo }
echo(
echo ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
echo To start your Gleam app:
echo     gleam run
echo(
echo If you installed PostgreSQL, you may need to start its service:
echo     pg_ctl -D %USERPROFILE%\scoop\persist\postgresql\data start
echo(
PAUSE
ENDLOCAL
