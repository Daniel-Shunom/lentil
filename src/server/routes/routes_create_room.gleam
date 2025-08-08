import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/otp/actor
import gleam/string
import global/ctx/ctx
import global/ctx/types as t
import global/functions.{id_generator}
import models/rooms/types/rooms.{type RoomCapacity}
import models/users/types/users.{UserId}
import pog
import server/sql
import wisp

pub fn handle_create_room(req: wisp.Request, ctx: ctx.Context) -> wisp.Response {
  use <- wisp.require_content_type(req, "application/json")
  use json <- wisp.require_json(req)
  case decode.run(json, room_decoder()) {
    Error(_) -> wisp.response(400)
    Ok(RoomReqs(roomid, ownerid, roomname, capacity, status)) -> {
      case
        sql.create_new_room(
          ctx.db_connection,
          roomid,
          ownerid,
          roomname,
          get_cap(capacity),
          status,
        )
      {
        Error(_) -> wisp.response(400)
        Ok(pog.Returned(_, rows)) -> {
          case rows {
            [] -> wisp.response(400)
            _ -> {
              t.NEWROOM(UserId(ownerid), capacity, roomname, roomid)
              |> actor.send(ctx.roomsupbox, _)
              let roomid = #("roomid", json.string(roomid))
              let roomname = #("roomname", json.string(roomname))
              list.new()
              |> list.prepend(roomid)
              |> list.prepend(roomname)
              |> json.object()
              |> json.to_string_tree()
              |> wisp.json_response(200)
            }
          }
        }
      }
    }
  }
}

fn room_decoder() -> decode.Decoder(RoomReqs) {
  use ownerid <- decode.field("userid", decode.string)
  use roomname <- decode.field("roomname", decode.string)
  use cap <- decode.field("capacity", decode.string)
  decode.success(RoomReqs(
    roomid: id_generator("lntl-rm"),
    owner_id: ownerid,
    roomname: roomname,
    capacity: capacity(cap),
    status: "",
  ))
}

type RoomReqs {
  RoomReqs(
    roomid: String,
    owner_id: String,
    roomname: String,
    capacity: RoomCapacity,
    status: String,
  )
}

fn capacity(str: String) -> RoomCapacity {
  let capacity =
    string.trim(str)
    |> string.lowercase()
  case capacity {
    "small" -> rooms.SMALL
    "medium" -> rooms.MEDIUM
    "large" -> rooms.LARGE
    _ -> rooms.SMALL
  }
}

fn get_cap(cap: RoomCapacity) -> Int {
  case cap {
    rooms.SMALL -> rooms.small_cap
    rooms.MEDIUM -> rooms.medium_cap
    rooms.LARGE -> rooms.large_cap
  }
}
