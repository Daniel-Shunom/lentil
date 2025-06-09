import gleam/erlang/process
import gleam/option.{type Option}
import gleam/otp/actor
import lntl_server/lntl_workers/toolkit/worker_types as wt
import mist
import users/types/users

pub fn handle_websockets(req, _roomid: String) {
  mist.websocket(req, ws_handler, on_init, on_close)
}

fn ws_handler(
  _state: WsState,
  _connection: mist.WebsocketConnection,
  _message: mist.WebsocketMessage(wt.SessionOperationMessage),
) -> actor.Next(wt.SessionOperationMessage, WsState) {
  todo
}

fn on_init(
  _connection: mist.WebsocketConnection,
) -> #(WsState, Option(process.Selector(wt.SessionOperationMessage))) {
  todo
}

fn on_close(_state: WsState) -> Nil {
  todo
}

pub type WsState {
  WsState(
    user: users.User,
    user_subj: process.Subject(wt.SessionOperationMessage),
    room_subj: process.Subject(wt.RoomSessionMessage),
  )
}
