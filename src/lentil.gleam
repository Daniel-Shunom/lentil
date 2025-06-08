import dot_env
import dot_env/env
import gleam/erlang/process
import lntl_server/router/router.{router}
import mist
import wisp

fn initialize_env() -> Nil {
  dot_env.new()
  |> dot_env.set_path(".env")
  |> dot_env.set_debug(False)
  |> dot_env.load()
}

fn get_env(val_key: String) -> String {
  let assert Ok(secret) = env.get_string(val_key)
  secret
}

pub fn main() {
  wisp.configure_logger()
  initialize_env()
  let sec = get_env("SECRET")

  let assert Ok(_) =
    router(_, sec)
    |> mist.new()
    |> mist.port(8000)
    |> mist.start_http()

  process.sleep_forever()
}
