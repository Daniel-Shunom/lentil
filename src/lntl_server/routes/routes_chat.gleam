import gleam/bytes_tree
import gleam/dynamic/decode
import gleam/erlang/process

//import gleam/http
import gleam/http/response
import gleam/option.{type Option, None}
import gleam/otp/actor
import global/ctx/ctx
import global/ctx/types.{type SupMsg} as t
import lntl_server/sql
import mist
import pog
import users/types/users
import wisp

pub fn handle_websockets(req, roomid: String, userid: String, ctx: ctx.Context) {
  case sql.fetch_is_valid_message(ctx.db_connection, roomid, userid) {
    Error(_) -> {
      let btree =
        "Connection not allowed"
        |> bytes_tree.from_string()
      response.new(400)
      |> response.set_body(mist.Bytes(btree))
    }
    Ok(pog.Returned(_, value)) -> {
      case value {
        [] -> {
          let btree =
            "Connection not allowed"
            |> bytes_tree.from_string()

          response.new(400)
          |> response.set_body(mist.Bytes(btree))
        }
        [val, ..] -> {
          case val.in_room {
            False -> {
              let btree =
                "Connection not allowed"
                |> bytes_tree.from_string()

              response.new(400)
              |> response.set_body(mist.Bytes(btree))
            }
            True -> {
              let handler = fn(state, conn, msg) {
                ws_handler(state, conn, msg, ctx, roomid)
              }
              mist.websocket(req, handler, on_init(_, userid), on_close)
            }
          }
        }
      }
    }
  }
}

fn ws_handler(
  state: WsState,
  _connection: mist.WebsocketConnection,
  message: mist.WebsocketMessage(SupMsg),
  context: ctx.Context,
  roomid: String,
) -> actor.Next(SupMsg, WsState) {
  case message {
    mist.Closed | mist.Shutdown -> actor.Stop(process.Normal)
    mist.Text(msg_text) -> {
      t.MSG(state.userid, roomid, msg_text)
      |> actor.send(context.usersupbox, _)
      actor.continue(state)
    }
    _ -> actor.continue(state)
  }
}

fn on_init(
  _connection: mist.WebsocketConnection,
  userid: String,
) -> #(WsState, Option(process.Selector(SupMsg))) {
  // TODO -> validate this userid
  #(WsState(users.UserId(userid)), None)
}

fn on_close(_) -> Nil {
  wisp.log_info("closed web socket")
}

pub type WsState {
  WsState(userid: users.UserId)
}

pub fn dec_userid() -> decode.Decoder(String) {
  use userid <- decode.field("userid", decode.string)
  decode.success(userid)
}
