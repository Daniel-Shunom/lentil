import gleam/bool
import gleam/result
import gleam/string_tree
import server/pages/a_def.{
  bad_request, default_page, not_found, server_error, too_large,
}
import simplifile
import wisp.{type Response}

pub type ERRORS {
  SERVERERROR
  BADREQUEST
  NOTFOUND
  TOOLARGE
}

fn get_path(error: ERRORS) -> String {
  case error {
    SERVERERROR -> server_error
    BADREQUEST -> bad_request
    NOTFOUND -> not_found
    TOOLARGE -> too_large
  }
}

pub fn build_res(error: ERRORS, response: wisp.Response) -> wisp.Response {
  let default = default_page
  get_path(error)
  |> simplifile.read()
  |> result.unwrap(default)
  |> string_tree.from_string()
  |> wisp.html_body(response, _)
}

pub fn define_request(handle_request: fn() -> Response) -> Response {
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
