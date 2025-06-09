import gleam/erlang/process
import global/functions.{get_env, initialize_env}
import lntl_server/router/router.{router}
import mist
import wisp

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
