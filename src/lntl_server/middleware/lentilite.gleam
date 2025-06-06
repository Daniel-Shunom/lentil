import gleam/bool
import lntl_server/middleware/helpers.{
  BADREQUEST, NOTFOUND, SERVERERROR, TOOLARGE, build_res,
}
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

fn define_request(handle_request: fn() -> Response) -> Response {
  let res = handle_request()
  use <- bool.guard(when: res.body != wisp.Empty, return: res)
  case res.status {
    404 | 405 -> build_res(NOTFOUND, res)
    400 | 422 -> build_res(BADREQUEST, res)
    413 -> build_res(TOOLARGE, res)
    500 -> build_res(SERVERERROR, res)
    _ -> res
  }
}
