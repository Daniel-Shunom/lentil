import lntl_server/routes/routes_auth
import wisp

pub fn server_routing(req: wisp.Request) -> wisp.Response {
  case wisp.path_segments(req) {
    ["auth", "signin"] -> routes_auth.handle_auth_signin(req)
    ["auth", "signout"] -> routes_auth.handle_auth_signout(req)
    _ -> wisp.response(500)
  }
}
