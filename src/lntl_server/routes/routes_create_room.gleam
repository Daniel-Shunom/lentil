import gleam/dynamic/decode
import gleam/otp/actor
import gleam/string
import global/ctx/ctx
import global/functions.{id_generator}
import lntl_server/sql
import rooms/types/rooms.{type RoomCapacity}
import users/types/users.{UserId}
import wisp

pub fn handle_create_room(req: wisp.Request, ctx: ctx.Context) -> wisp.Response {
  use <- wisp.require_content_type(req, "application/json")
  use json <- wisp.require_json(req)
  case decode.run(json, room_decoder()) {
    Error(_) -> wisp.response(400)
    Ok(RoomReqs(id, ownerid, roomname, capacity, status)) -> {
      case
        sql.create_new_room(
          ctx.db_connection,
          id,
          ownerid,
          roomname,
          get_cap(capacity),
          status,
        )
      {
        Error(_) -> wisp.response(400)
        Ok(_) -> {
          ctx.NEWROOM(UserId(ownerid), capacity, roomname)
          |> actor.send(ctx.roomsupbox, _)
          wisp.response(200)
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
    id: id_generator("lntl-rm"),
    owner_id: ownerid,
    roomname: roomname,
    capacity: capacity(cap),
    status: "",
  ))
}

type RoomReqs {
  RoomReqs(
    id: String,
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
