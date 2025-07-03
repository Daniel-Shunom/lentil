import gleam/function
import gleam/bytes_tree
import gleam/dynamic/decode
import gleam/erlang/process

//import gleam/http
import gleam/http/response
import gleam/option.{type Option, None,Some}
import gleam/otp/actor
import global/ctx/ctx
import global/ctx/types as t
import lntl_server/lntl_workers/toolkit/worker_types as wt
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
              
              // Send a message to add user proc to broadcast
              mist.websocket(req, handler, on_init(_, userid, roomid, ctx), on_close)
            }
          }
        }
      }
    }
  }
}

fn ws_handler(
  state: WsState,
  connection: mist.WebsocketConnection,
  message: mist.WebsocketMessage(wt.RoomMessageStream),
  context: ctx.Context,
  roomid: String,
) -> actor.Next(wt.RoomMessageStream, WsState) {
  case message {
    mist.Closed | mist.Shutdown -> actor.Stop(process.Normal)
    mist.Text(msg_text) -> {
      t.MSG(state.userid, roomid, msg_text)
      |> actor.send(context.usersupbox, _)
      actor.continue(state)
    }

    mist.Custom(room_msg) -> {
      let msg = room_msg.message.message_content
      let _ = mist.send_text_frame(connection, "INCOMING MESSAGE: " <> msg)
      case mist.send_text_frame(connection, msg) {
        Ok(_) -> actor.continue(state)
        Error(reason) -> {
          echo reason
          actor.continue(state)
        }
      }

      actor.continue(state)
    }
    _ -> actor.continue(state)
  }
}

fn on_init(
  connection: mist.WebsocketConnection,
  userid: String,
  roomid: String,
  context: ctx.Context
) -> #(WsState, Option(process.Selector(wt.RoomMessageStream))) {
  // TODO -> validate this userid
  let _ = mist.send_text_frame(connection, "WELCOME TO ROOM " <> roomid)
  t.ADDTOBROADCAST(users.UserId(userid), roomid)
  |> actor.send(context.roomsupbox, _)

  let shipment = process.new_subject()
  t.GETUSERSESSION(users.UserId(userid), shipment)
  |> process.send(context.usersupbox, _)

  case process.receive(shipment, 1000) {
    Error(_) -> {
      wisp.log_alert("CANNOT RETRIEVE INBOX STREAM")
      #(WsState(users.UserId(userid)), None)
    }
    Ok(package) -> {
      let reciever = 
        process.new_selector()
        |> process.selecting(package.1, function.identity)
      wisp.log_notice("SUCCESSFULLY RETREIVED USER PACKAGE")
      echo package
      #(WsState(users.UserId(userid)), Some(reciever))
    }
  }
}

fn on_close(_) -> Nil {
  wisp.configure_logger()
  wisp.log_info("closed web socket")
}

pub type WsState {
  WsState(userid: users.UserId)
}

pub fn dec_userid() -> decode.Decoder(String) {
  use userid <- decode.field("userid", decode.string)
  decode.success(userid)
}
