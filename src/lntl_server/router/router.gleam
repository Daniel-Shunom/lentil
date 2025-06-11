import gleam/http
import global/ctx/ctx
import lntl_server/middleware/lentilite.{middleware}
import lntl_server/routes/routes_auth
import lntl_server/routes/routes_chat
import lntl_server/routes/routes_create_room
import lntl_server/routes/routes_create_user
import lntl_server/routes/routes_delete_room
import lntl_server/routes/routes_profile
import wisp
import wisp/wisp_mist

pub fn router(req, str: String, ctx: ctx.Context) {
  let handler = server_routing(_, ctx)
  case wisp.path_segments(req) {
    ["rooms", "joinroom", roomid] -> {
      routes_chat.handle_websockets(req, roomid)
    }
    _ -> wisp_mist.handler(handler, str)(req)
  }
}

fn server_routing(req: wisp.Request, ctx: ctx.Context) -> wisp.Response {
  use req, ctx <- middleware(req, ctx)
  case wisp.path_segments(req) {
    [] -> wisp.accepted()

    ["auth", "signin"] -> {
      use <- wisp.require_method(req, http.Post)
      routes_auth.handle_auth_signin(req, ctx)
    }

    ["auth", "signout"] -> {
      use <- wisp.require_method(req, http.Post)
      routes_auth.handle_auth_signout(req, ctx)
    }

    ["user", "signup"] -> {
      use <- wisp.require_method(req, http.Post)
      routes_create_user.handle_create_user(req, ctx)
    }

    ["user", "profile", _sessionid] -> {
      use <- wisp.require_method(req, http.Get)
      routes_profile.handle_get_profile(req, ctx)
    }

    ["rooms", "newroom"] -> {
      use <- wisp.require_method(req, http.Post)
      routes_create_room.handle_create_room(req, ctx)
    }

    ["rooms", "deleteroom"] -> {
      use <- wisp.require_method(req, http.Delete)
      routes_delete_room.handle_delete_room(req, ctx)
    }

    _ -> wisp.bad_request()
  }
}
