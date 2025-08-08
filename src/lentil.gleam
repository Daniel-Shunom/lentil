// import gleam/function
import gleam/erlang/process
import global/ctx/ctx.{get_context, init_supstate, rmctxprx, sup_ctx}
import global/functions.{connect_lentildb, get_env, initialize_env}
import mist
import server/router/router.{router}
import utils/init/init_router as init
import utils/monitor_router.{monitor_router}
import utils/sys_utils/sys_utilsg as sys
import wisp

pub fn main() {
  wisp.configure_logger()
  initialize_env()
  let connection = connect_lentildb()
  let roombox_subj = rmctxprx()
  let supstate = init_supstate(roombox_subj)
  let sec = get_env("SECRET")
  let server_monitor = init.init_global_router_actor()
  let _ =
    process.start(
      fn() {
        let _ = sys.observer()
        let _ = sys.start_osmon(server_monitor)
      },
      False,
    )

  let ctx =
    get_context(
      connection,
      roombox_subj,
      sup_ctx(supstate, connection),
      server_monitor,
    )

  let _ =
    process.start(
      fn() {
        let assert Ok(_) =
          monitor_router(_, sec, server_monitor)
          |> mist.new()
          |> mist.port(5050)
          |> mist.start_http()
        process.sleep_forever()
      },
      False,
    )

  let assert Ok(_) =
    router(_, sec, ctx)
    |> mist.new()
    |> mist.port(8000)
    |> mist.start_http()

  process.sleep_forever()
}
