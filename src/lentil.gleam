import gleam/erlang/process
import global/ctx/ctx.{get_context}
import global/functions.{connect_lentildb, get_env, initialize_env}
import lntl_server/router/router.{router}
import mist
import wisp

pub fn main() {
  wisp.configure_logger()
  initialize_env()
  let sec = get_env("SECRET")
  let ctx =
    connect_lentildb()
    |> get_context()

  let assert Ok(_) =
    router(_, sec, ctx)
    |> mist.new()
    |> mist.port(8000)
    |> mist.start_http()

  process.sleep_forever()
}
