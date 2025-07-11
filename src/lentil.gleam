import gleam/erlang/process
import global/ctx/ctx.{get_context, init_supstate, rmctxprx, sup_ctx}
import global/functions.{connect_lentildb, get_env, initialize_env}
import lntl_server/router/router.{router}
import mist
import wisp

pub fn main() {
  wisp.configure_logger()
  initialize_env()
  let connection = connect_lentildb()
  let roombox_subj = rmctxprx()
  let supstate = init_supstate(roombox_subj)
  let sec = get_env("SECRET")
  let ctx = {
    connection
    |> get_context(roombox_subj, sup_ctx(supstate, connection))
  }

  let assert Ok(_) =
    router(_, sec, ctx)
    |> mist.new()
    |> mist.port(8000)
    |> mist.start_http()

  process.sleep_forever()
}
