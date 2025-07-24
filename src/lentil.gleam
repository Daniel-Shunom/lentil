import gleam/erlang/process
import global/ctx/ctx.{get_context, init_supstate, rmctxprx, sup_ctx}
import global/functions.{connect_lentildb, get_env, initialize_env}
import lntl_frontline/init/init_router as init
import lntl_server/router/router.{router}
import lntl_frontline/monitor_router.{monitor_router}
import mist
import wisp

pub fn main() {
  wisp.configure_logger()
  initialize_env()
  let connection = connect_lentildb()
  let roombox_subj = rmctxprx()
  let supstate = init_supstate(roombox_subj)
  let sec = get_env("SECRET")
  let server_monitor = init.init_global_router_actor()
  let ctx =
    get_context(
      connection,
      roombox_subj,
      sup_ctx(supstate, connection),
      server_monitor,
    )

  let assert Ok(_) =
    router(_, sec, ctx)
    |> mist.new()
    |> mist.port(8000)
    |> mist.start_http()

  let assert Ok(_) =  
    monitor_router(_, sec)
    |> mist.new()
    |> mist.port(5000)
    |> mist.start_http()

  process.sleep_forever()
}
