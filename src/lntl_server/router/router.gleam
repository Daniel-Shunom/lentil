import gleam/http
import lntl_server/middleware/lentilite.{middleware}
import lntl_server/routes/routes_auth
import lntl_server/routes/routes_chat
import lntl_server/routes/routes_create_room
import lntl_server/routes/routes_create_user
import lntl_server/routes/routes_delete_room
import lntl_server/routes/routes_profile
import wisp
import wisp/wisp_mist

pub fn router(req, str: String) {
  case wisp.path_segments(req) {
    ["rooms", "joinroom", roomid] -> {
      routes_chat.handle_websockets(req, roomid)
    }
    _ -> wisp_mist.handler(server_routing, str)(req)
  }
}

fn server_routing(req: wisp.Request) -> wisp.Response {
  use req <- middleware(req)
  case wisp.path_segments(req) {
    [] -> wisp.accepted()

    ["auth", "signin"] -> {
      use <- wisp.require_method(req, http.Post)
      routes_auth.handle_auth_signin(req)
    }

    ["auth", "signout"] -> {
      use <- wisp.require_method(req, http.Post)
      routes_auth.handle_auth_signout(req)
    }

    ["user", "signup"] -> {
      use <- wisp.require_method(req, http.Post)
      routes_create_user.handle_create_user(req)
    }

    ["user", "profile", _sessionid] -> {
      use <- wisp.require_method(req, http.Get)
      routes_profile.handle_get_profile(req)
    }

    ["rooms", "newroom"] -> {
      use <- wisp.require_method(req, http.Post)
      routes_create_room.handle_create_room(req)
    }

    ["rooms", "deleteroom"] -> {
      use <- wisp.require_method(req, http.Delete)
      routes_delete_room.handle_delete_room(req)
    }

    _ -> wisp.bad_request()
  }
}
