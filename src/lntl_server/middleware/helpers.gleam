import gleam/result
import gleam/string_tree
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
    SERVERERROR -> ""
    BADREQUEST -> ""
    NOTFOUND -> ""
    TOOLARGE -> ""
  }
}

pub fn build_res(error: ERRORS, response: wisp.Response) -> wisp.Response {
  let default = "<p>Error: Something went wrong.</p>"
  get_path(error)
  |> simplifile.read()
  |> result.unwrap(default)
  |> string_tree.from_string()
  |> wisp.html_body(response, _)
}
