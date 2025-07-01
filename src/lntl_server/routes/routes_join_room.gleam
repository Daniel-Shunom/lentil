import gleam/bool
import gleam/dynamic/decode
import global/ctx/ctx
import lntl_server/sql
import wisp

pub fn join_room(req: wisp.Request, ctx: ctx.Context) -> wisp.Response {
  use <- wisp.require_content_type(req, "application/json")
  use json <- wisp.require_json(req)
  case decode.run(json, req_decoder()) {
    Error(_) -> wisp.bad_request()
    Ok(Credentials(_uname, userid, roomid, auth)) -> {
      use <- bool.guard(!auth, wisp.bad_request())
      // validation logic here
      case sql.add_user_to_room(ctx.db_connection, roomid, userid) {
        Error(_) -> wisp.bad_request()
        Ok(_) -> wisp.accepted()
      }
    }
  }
}

fn req_decoder() {
  use username <- decode.field("username", decode.string)
  use userid <- decode.field("userid", decode.string)
  use roomid <- decode.field("roomid", decode.string)
  use authenticated <- decode.field("authenticated", decode.bool)
  decode.success(Credentials(username, userid, roomid, authenticated))
}

type Credentials {
  Credentials(
    username: String,
    userid: String,
    roomid: String,
    authenticated: Bool,
  )
}
