import gleam/erlang/process
import lntl_server/router/router.{router}
import mist
import wisp
import zyx/zyx.{get_env, initialize_env}

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
