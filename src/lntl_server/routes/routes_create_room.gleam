import gleam/dynamic/decode
import global/ctx/ctx
import global/functions.{id_generator}
import lntl_server/lntl_workers/toolkit/worker_functions as wf
import lntl_server/sql
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
          capacity,
          status,
        )
      {
        Error(_) -> wisp.response(400)
        Ok(val) -> {
          case val.rows {
            [] -> wisp.bad_request()
            [room, ..] -> {
              room.id
              todo as "create permanent running process"
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
  use capacity <- decode.field("capacity", decode.int)
  decode.success(RoomReqs(
    id: id_generator("lntl-rm"),
    owner_id: ownerid,
    roomname: roomname,
    capacity: capacity,
    status: "",
  ))
}

type RoomReqs {
  RoomReqs(
    id: String,
    owner_id: String,
    roomname: String,
    capacity: Int,
    status: String,
  )
}
