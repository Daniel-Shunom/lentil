import dev/stress_test/dev_consts as dc
import gleam/dynamic/decode
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/json
import gleam/list

pub fn build_user(first: String, last: String) {
  let fname = #("firstname", json.string(first))
  let lname = #("lastname", json.string(last))
  let uname = #("username", json.string(first <> last))
  let paswd = #("password", json.string(first <> last <> "123"))
  let pnnss = #("pronounssingular", json.string("they"))
  let pnnpp = #("pronounsplural", json.string("them"))
  let day = #("day", json.string("1"))
  let month = #("month", json.string("June"))
  let year = #("year", json.string("2018"))
  let gender = #("gender", json.string("non-binary"))

  let req =
    list.new()
    |> list.prepend(gender)
    |> list.prepend(year)
    |> list.prepend(month)
    |> list.prepend(day)
    |> list.prepend(pnnpp)
    |> list.prepend(pnnss)
    |> list.prepend(paswd)
    |> list.prepend(uname)
    |> list.prepend(lname)
    |> list.prepend(fname)
    |> json.object()
    |> json.to_string()

  request.new()
  |> request.set_body(req)
  |> request.set_host(dc.host)
  |> request.set_path(dc.endpoint_signup)
  |> request.set_method(http.Post)
  |> httpc.send()
}

pub fn login_devuser(user: DevUser) {
  let uname = #("username", json.string(user.username))
  let paswd = #("password", json.string(user.passwd))

  list.new()
  |> list.prepend(paswd)
  |> list.prepend(uname)
  |> json.object()
  |> json.to_string()
  |> request.set_body(request.new(), _)
  |> request.set_host(dc.host)
  |> request.set_path(dc.endpoint_signin)
  |> request.set_method(http.Post)
  |> httpc.send()
}

pub fn dev_userdecoder() {
  use uname <- decode.field("username", decode.string)
  use userid <- decode.field("userid", decode.string)
  decode.success(DevUser(uname, userid, uname <> "123"))
}

pub type DevUser {
  DevUser(username: String, userid: String, passwd: String)
}
