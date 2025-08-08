import gleam/dynamic/decode
import gleam/otp/actor
import gleam/otp/task
import global/ctx/ctx
import global/ctx/types as t
import server/sql
import wisp

pub fn handle_delete_room(req: wisp.Request, ctx: ctx.Context) -> wisp.Response {
  use <- wisp.require_content_type(req, "application/json")
  use json <- wisp.require_json(req)
  case decode.run(json, requirements_decoder()) {
    Error(_) -> wisp.response(400)
    Ok(Requirements(userid, roomsessionid)) -> {
      case sql.delete_room(ctx.db_connection, roomsessionid, userid) {
        Error(_) -> wisp.bad_request()
        Ok(_) -> {
          let task =
            fn() {
              t.DELROOM(roomsessionid)
              |> actor.send(ctx.roomsupbox, _)
            }
            |> task.async()
          case task.try_await(task, 10_000) {
            Error(_) -> wisp.response(400)
            Ok(_) -> wisp.response(200)
          }
        }
      }
    }
  }
}

type Requirements {
  Requirements(userid: String, roomid: String)
}

fn requirements_decoder() -> decode.Decoder(Requirements) {
  use user_id <- decode.field("userid", decode.string)
  use room_id <- decode.field("roomsessionid", decode.string)
  decode.success(Requirements(user_id, room_id))
}
