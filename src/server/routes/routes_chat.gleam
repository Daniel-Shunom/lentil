import gleam/bool
import gleam/bytes_tree
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/function
import gleam/http/response
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import global/ctx/ctx
import global/ctx/types as t
import global/functions as gf
import mist
import models/users/types/users
import pog
import server/sql
import server/workers/shared/shared_types as sm
import utils/msg_types as mt
import wisp

pub fn handle_websockets(req, roomid: String, userid: String, ctx: ctx.Context) {
  {
    let err =
      response.new(400)
      |> response.set_body(
        "connection not allowed"
        |> bytes_tree.from_string()
        |> mist.Bytes(),
      )
    use pog.Returned(_, value) <- result.try(
      sql.fetch_is_valid_message(ctx.db_connection, roomid, userid)
      |> result.replace_error(err),
    )
    use extracted <- result.try(
      list.first(value)
      |> result.replace_error(err),
    )
    use <- bool.guard(!extracted.in_room, Error(err))
    let handler = fn(state, conn, msg) {
      ws_handler(state, conn, msg, ctx, roomid)
    }
    mist.websocket(req, handler, on_init(_, userid, roomid, ctx), on_close(
      _,
      roomid,
      ctx,
    ))
    |> Ok
  }
  |> result.unwrap_both()
}

fn ws_handler(
  state: WsState,
  connection: mist.WebsocketConnection,
  message: mist.WebsocketMessage(sm.RoomMessageStream),
  context: ctx.Context,
  roomid: String,
) -> actor.Next(sm.RoomMessageStream, WsState) {
  case message {
    mist.Closed | mist.Shutdown -> actor.Stop(process.Normal)
    mist.Text(msg_text) -> {
      t.MSG(state.userid, roomid, msg_text)
      |> actor.send(context.usersupbox, _)
      actor.continue(state)
    }

    mist.Custom(room_msg) -> {
      case room_msg {
        sm.Incoming(_, msg) -> {
          let msg = msg.message_content
          case mist.send_text_frame(connection, "INCOMING MESSAGE: " <> msg) {
            Ok(_) -> actor.continue(state)
            Error(reason) -> {
              echo reason
              actor.continue(state)
            }
          }
        }
        _ -> {
          wisp.log_alert("FORBIDDEN OPERATION")
          actor.continue(state)
        }
      }
    }
    _ -> actor.continue(state)
  }
}

fn on_init(
  connection: mist.WebsocketConnection,
  userid: String,
  roomid: String,
  context: ctx.Context,
) -> #(WsState, Option(process.Selector(sm.RoomMessageStream))) {
  // TODO -> validate this userid
  let _ = mist.send_text_frame(connection, "WELCOME TO ROOM " <> roomid)
  let ws_inbox = process.new_subject()
  t.ADDTOBROADCAST(users.UserId(userid), roomid, ws_inbox)
  |> actor.send(context.roomsupbox, _)

  mt.ClientRouterMessage(mt.CLIENTRoomEvent(
    userid: userid,
    roomid: roomid,
    authenticated: True,
    event_type: mt.JOINROOMSESSION,
    lntl_time: gf.get_timestamp(),
  ))
  |> actor.send(context.server_monitor, _)

  let shipment = process.new_subject()
  t.GETUSERSESSION(users.UserId(userid), shipment)
  |> process.send(context.usersupbox, _)

  case process.receive(shipment, 1000) {
    Error(_) -> {
      wisp.log_alert("CANNOT RETRIEVE INBOX STREAM")
      #(WsState(users.UserId(userid)), None)
    }
    Ok(_package) -> {
      let reciever =
        process.new_selector()
        |> process.selecting(ws_inbox, function.identity)
      wisp.log_notice("SUCCESSFULLY RETREIVED USER PACKAGE")
      #(WsState(users.UserId(userid)), Some(reciever))
    }
  }
}

fn on_close(state: WsState, roomid: String, context: ctx.Context) -> Nil {
  mt.ClientRouterMessage(mt.CLIENTRoomEvent(
    userid: state.userid.id,
    roomid: roomid,
    authenticated: True,
    event_type: mt.LEAVEROOMSESSION,
    lntl_time: gf.get_timestamp(),
  ))
  |> actor.send(context.server_monitor, _)

  t.REMOVEFROMBROADCAST(state.userid, roomid)
  |> actor.send(context.roomsupbox, _)
  wisp.log_info("closed web socket")
}

pub type WsState {
  WsState(userid: users.UserId)
}

pub fn dec_userid() -> decode.Decoder(String) {
  use userid <- decode.field("userid", decode.string)
  decode.success(userid)
}
