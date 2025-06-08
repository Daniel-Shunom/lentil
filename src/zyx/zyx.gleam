import dot_env
import dot_env/env

pub fn initialize_env() -> Nil {
  dot_env.new()
  |> dot_env.set_path(".env")
  |> dot_env.set_debug(False)
  |> dot_env.load()
}

pub fn get_env(val_key: String) -> String {
  let assert Ok(secret) = env.get_string(val_key)
  secret
}
