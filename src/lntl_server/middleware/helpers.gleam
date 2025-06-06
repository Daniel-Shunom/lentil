import gleam/result
import gleam/string_tree
import lntl_server/pages/a_def.{
  bad_request, default_page, not_found, server_error, too_large,
}
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
