import gleam/dynamic/decode
import global/functions.{connect_lentildb}
import lntl_server/sql
import wisp

pub fn handle_delete_room(req: wisp.Request) -> wisp.Response {
  use <- wisp.require_content_type(req, "application/json")
  use json <- wisp.require_json(req)
  case decode.run(json, requirements_decoder()) {
    Error(_) -> wisp.response(400)
    Ok(Requirements(userid, roomid)) -> {
      case sql.delete_room(connect_lentildb(), roomid, userid) {
        Error(_) -> wisp.bad_request()
        Ok(_) -> {
          todo as "shutdown room process"
          wisp.response(200)
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
  use room_id <- decode.field("roomid", decode.string)
  decode.success(Requirements(user_id, room_id))
}
