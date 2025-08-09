import gleam/http
import gleam/list
import gleam/json
import gleam/http/request

pub fn build_user(first: String, last: String) {
  let fname = #("firstname", json.string(first))
  let lname = #("lastname", json.string(last))
  let uname = #("username", json.string(first <> last))
  let paswd = #("password", json.string(first <> last <> "123"))
  let pnnss = #("pronounssingular", json.string("they"))
  let pnnpp = #("pronounsplural", json.string("them"))
  let day   = #("day", json.string("1"))
  let month = #("month", json.string("June"))
  let year  = #("year", json.string("2018"))
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
  
  request.new() 
  |> request.set_body(req)
  |> request.set_host("http://localhost:3000/auth/signup")
  |> request.set_method(http.Post)
  |> request.set_port(3000)
}
