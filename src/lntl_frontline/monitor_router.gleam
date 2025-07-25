import gleam/erlang/process
import lntl_frontline/stream/evstream
import gleam/http
import wisp
import wisp/wisp_mist
import lntl_frontline/msg_types as mt

pub fn monitor_router(req, sec: String, monitor: process.Subject(mt.GlobalMonitorMessage(a))) {
  case wisp.path_segments(req) {
    [] -> evstream.stream_serverside_events(req, monitor)
    _ -> wisp_mist.handler(monitor_routing, sec)(req)
  }
}

fn monitor_routing(req: wisp.Request) -> wisp.Response {
  case wisp.path_segments(req) {
    ["server", "status"] -> {
      use <- wisp.require_method(req, http.Get)
      wisp.accepted()
    }

    _ -> wisp.bad_request()
  }
}
