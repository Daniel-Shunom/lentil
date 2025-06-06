import gleam/result
import gleam/string_tree
import lntl_server/pages/a_def.{default_page}
import simplifile
import wisp

pub type ERRORS {
  SERVERERROR
  BADREQUEST
  NOTFOUND
  TOOLARGE
}

fn get_path(error: ERRORS) -> String {
  case error {
    SERVERERROR -> "src/lntl_server/pages/server_error.html"
    BADREQUEST -> "src/lntl_server/pages/bad_request.html"
    NOTFOUND -> "src/lntl_server/pages/not_found.html"
    TOOLARGE -> "src/lntl_server/pages/too_large.html"
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
