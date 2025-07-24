import wisp/wisp_mist
import wisp
import gleam/http

pub fn monitor_router(req, sec: String) {
  wisp_mist.handler(monitor_routing, sec)(req)
}

fn monitor_routing(req: wisp.Request) -> wisp.Response {
  case wisp.path_segments(req) {
    [] -> {
      use <- wisp.require_method(req, http.Get)
      wisp.response(202)
    }

    ["server", "status"] -> { 
      use <- wisp.require_method(req, http.Get)
      wisp.accepted() 
    }

    _ -> wisp.bad_request()
  }
}