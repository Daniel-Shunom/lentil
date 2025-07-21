echo "Welcome to the lentil Installation Guide!"


#!/bin/bash
set -e

# 1. Check OS and install Gleam and Erlang
if ! command -v gleam &> /dev/null; then
  echo "Gleam not found. Installing Gleam..."
  if [ "$(uname)" = "Darwin" ]; then
    brew update && brew install gleam      # MacOS
  elif [ -x "$(command -v apt-get)" ]; then
    sudo apt-get update
    sudo apt-get install -y curl erlang
    curl -L https://github.com/gleam-lang/gleam/releases/latest/download/gleam-x86_64-unknown-linux-gnu.tar.gz | tar xz
    sudo mv gleam /usr/local/bin/
  else
    echo "Please install Gleam and Erlang manually from https://gleam.run/getting-started/installing/"
    exit 1
  fi
else
  echo "Gleam already installed."
fi

if ! command -v erl &> /dev/null; then
  echo "Erlang not found. Please install Erlang for Gleam to work."
  exit 1
else
  echo "Erlang found."
fi

# 2. Set up Gleam project dependencies and build
if [ ! -d my_gleam_app ]; then
  gleam new my_gleam_app
fi
cd my_gleam_app

# Example adding dependencies. Adjust as needed for your app.
gleam add envoy argv gleam_erlang

# Build the application
gleam build

# 3. Check and install PostgreSQL
if ! command -v psql &> /dev/null; then
  echo "PostgreSQL not found."
  read -p "Do you want to install PostgreSQL? (y/n): " pg_install
  if [ "$pg_install" = "y" ]; then
    if [ -x "$(command -v apt-get)" ]; then
      sudo apt-get install -y postgresql
    elif [ "$(uname)" = "Darwin" ]; then
      brew install postgresql
    else
      echo "Please install PostgreSQL manually."
      exit 1
    fi
    echo "PostgreSQL installed."
  else
    echo "Please manually configure your PostgreSQL database and ensure it's available to your app."
    exit 1
  fi
else
  echo "PostgreSQL is already installed."
fi

# 4. Set environment variable (e.g., DATABASE_URL)
export DATABASE_URL="postgres://user:password@localhost:5432/my_db"
echo 'export DATABASE_URL="postgres://user:password@localhost:5432/my_db"' >> ~/.bashrc

# 5. Show how to use env variable in Gleam app (envoy dependency)
cat <<EOL

In your Gleam code, use the 'envoy' package to read env variables, e.g.:
import envoy
pub fn main() {
  let db_url = envoy.get("DATABASE_URL")
  // use db_url in your app
}

EOL

# 6. Launch commands
echo "To start your Gleam application:"
echo "cd my_gleam_app"
echo "gleam run"
