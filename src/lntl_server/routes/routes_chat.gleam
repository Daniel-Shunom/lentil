import gleam/erlang/process
import gleam/option.{type Option}
import gleam/otp/actor
import global/ctx/ctx.{type SupMsg}
import mist
import users/types/users
import wisp

pub fn handle_websockets(req, roomid: String, ctx: ctx.Context) {
  let handler = fn(state, conn, msg) {
    ws_handler(state, conn, msg, ctx, roomid)
  }
  mist.websocket(req, handler, on_init, on_close)
}

fn ws_handler(
  state: WsState,
  _connection: mist.WebsocketConnection,
  message: mist.WebsocketMessage(SupMsg),
  context: ctx.Context,
  roomid: String,
) -> actor.Next(SupMsg, WsState) {
  case message {
    mist.Closed | mist.Shutdown -> {
      todo
    }
    mist.Text(msg_text) -> {
      ctx.MSG(state.user.user_id, roomid, msg_text)
      |> actor.send(context.usersupbox, _)
      actor.continue(state)
    }
    _ -> {
      todo
    }
  }
}

fn on_init(
  _connection: mist.WebsocketConnection,
) -> #(WsState, Option(process.Selector(SupMsg))) {
  todo
}

fn on_close(_) -> Nil {
  wisp.log_info("Closed socket")
}

pub type WsState {
  WsState(user: users.User)
}
