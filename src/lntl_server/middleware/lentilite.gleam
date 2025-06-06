import lntl_server/middleware/helpers.{define_request}
import wisp.{type Request, type Response}

pub fn middleware(
  request: Request,
  handle_request: fn(Request) -> Response,
) -> Response {
  let req = wisp.method_override(request)
  use <- wisp.log_request(req)
  use <- wisp.rescue_crashes()
  use req <- wisp.handle_head(req)
  use <- define_request()
  handle_request(req)
}
