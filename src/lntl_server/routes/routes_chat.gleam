import gleam/erlang/process
import gleam/http
import gleam/option.{type Option}
import gleam/otp/actor
import lntl_server/lntl_workers/toolkit/worker_types as wt
import mist
import wisp/wisp_mist

pub fn handle_websockets(req) {
  mist.websocket(req, ws_handler, on_init, on_close)
}

fn ws_handler(
  state: wt.UserSession,
  connection: mist.WebsocketConnection,
  message: mist.WebsocketMessage(wt.SessionOperationMessage),
) -> actor.Next(wt.SessionOperationMessage, wt.UserSession) {
  todo
}

fn on_init(
  connection: mist.WebsocketConnection,
) -> #(wt.UserSession, Option(process.Selector(wt.SessionOperationMessage))) {
  todo
}

fn on_close(state: wt.UserSession) -> Nil {
  todo
}
