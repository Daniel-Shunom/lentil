import gleam/http
import lntl_server/routes/routes_auth
import lntl_server/routes/routes_create_user
import wisp

pub fn server_routing(req: wisp.Request) -> wisp.Response {
  case wisp.path_segments(req) {
    ["auth", "signin"] -> {
      use <- wisp.require_method(req, http.Post)
      routes_auth.handle_auth_signin(req)
    }
    ["auth", "signout"] -> {
      use <- wisp.require_method(req, http.Post)
      routes_auth.handle_auth_signout(req)
    }
    ["signup"] -> {
      use <- wisp.require_method(req, http.Post)
      routes_create_user.handle_create_user(req)
    }
    _ -> wisp.response(500)
  }
}
