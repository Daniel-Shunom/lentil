
#!/bin/bash
set -e

echo "Welcome to the lentil Installation Guide!"

# 0. Set installation directory inside user's home directory
INSTALL_DIR="$HOME/lentilapp"

# 1. Check OS and install Gleam and Erlang
if ! command -v gleam &> /dev/null; then
  echo "Gleam not found. Installing Gleam..."
  if [[ "$(uname)" == "Darwin" ]]; then
    brew update && brew install gleam      # MacOS
  elif command -v apt-get &> /dev/null; then
    sudo apt-get update
    sudo apt-get install -y curl erlang
    curl -L https://github.com/gleam-lang/gleam/releases/latest/download/gleam-x86_64-unknown-linux-gnu.tar.gz | tar xz
    sudo mv gleam /usr/local/bin/
  elif command -v pacman &> /dev/null; then
    sudo pacman -Syu --noconfirm
    sudo pacman -S --noconfirm curl erlang
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

# 2. Clone app repo if not present and build
if [ ! -d "$INSTALL_DIR" ]; then
  git clone https://github.com/Daniel-Shunom/lentil.git "$INSTALL_DIR"
fi
cd "$INSTALL_DIR"

# Build the application
gleam build

# 3. Check and install PostgreSQL
if ! command -v psql &> /dev/null; then
  echo "PostgreSQL not found."
  read -rp "Do you want to install PostgreSQL? (y/n): " pg_install
  if [[ "$pg_install" == "y" ]]; then
    if command -v apt-get &> /dev/null; then
      sudo apt-get install -y postgresql
    elif [[ "$(uname)" == "Darwin" ]]; then
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

# 4. Apply schema from priv/schema.sql
DB_NAME="your_database_name_here"   # <<< Replace with your database name or make this dynamic

echo "Applying baseline schema from priv/schema.sql to database '$DB_NAME'..."
psql -d "$DB_NAME" -f priv/schema.sql
echo "Schema setup complete."

# 5. Set environment variable (e.g., DATABASE_URL)
export DATABASE_URL="postgres://user:password@localhost:5432/$DB_NAME"
if ! grep -qxF "export DATABASE_URL=\"postgres://user:password@localhost:5432/$DB_NAME\"" ~/.bashrc; then
  echo "export DATABASE_URL=\"postgres://user:password@localhost:5432/$DB_NAME\"" >> ~/.bashrc
fi

# 6. Show how to use env variable in Gleam app (envoy dependency)
cat <<EOL
In your Gleam code, use the 'envoy' package to read environment variables, e.g.:
import envoy
pub fn main() {
  let db_url = envoy.get("DATABASE_URL")
  // use db_url in your app
}
EOL

# 7. Launch commands
echo "To start your Gleam application:"
echo "cd lentilapp"
echo "gleam run"

