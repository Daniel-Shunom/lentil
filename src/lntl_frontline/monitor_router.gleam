import wisp/wisp_mist
import wisp

pub fn monitor_router(req, sec: String) {
  wisp_mist.handler(monitor_routing, sec)(req)
}

fn monitor_routing(req: wisp.Request) -> wisp.Response {
  case wisp.path_segments(req) {
    [] -> { todo }
    ["/", "status"] -> { todo }
    _ -> wisp.bad_request()
  }
}