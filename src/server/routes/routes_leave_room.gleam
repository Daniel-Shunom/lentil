import gleam/dynamic/decode
import gleam/otp/actor
import gleam/result
import global/ctx/ctx
import global/functions
import utils/msg_types as mt
import server/sql
import wisp

pub fn handle_leave_room(req: wisp.Request, ctx: ctx.Context) -> wisp.Response {
  use <- wisp.require_content_type(req, "applicaation/json")
  use json <- wisp.require_json(req)
  case decode.run(json, req_decoder()) {
    Error(_) -> wisp.bad_request()
    Ok(Credentials(_username, userid, roomid)) -> {
      mt.ClientRouterMessage(mt.CLIENTRoomEvent(
        userid: userid,
        roomid: roomid,
        authenticated: True,
        event_type: mt.LEAVEROOM,
        lntl_time: functions.get_timestamp(),
      ))
      |> actor.send(ctx.server_monitor, _)
      sql.delete_user_from_room(ctx.db_connection, roomid, userid)
      |> result.map_error(fn(_) { wisp.bad_request() })
      |> result.unwrap_error(wisp.accepted())
    }
  }
}

fn req_decoder() {
  use username <- decode.field("username", decode.string)
  use userid <- decode.field("userid", decode.string)
  use roomid <- decode.field("roomid", decode.string)
  decode.success(Credentials(username, userid, roomid))
}

type Credentials {
  Credentials(username: String, userid: String, roomid: String)
}
