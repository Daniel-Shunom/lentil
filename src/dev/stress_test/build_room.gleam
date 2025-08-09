import gleam/http
import gleam/http/request
import gleam/list
import gleam/json

pub fn build_room(user_id: String) {
  let roomname = #("roomname", json.string("room"))
  let userid = #("userid", json.string(user_id))
  let capacity = #("capacity", json.string("medium"))

  let req = 
    list.new()
    |> list.prepend(capacity)
    |> list.prepend(roomname)
    |> list.prepend(userid)
    |> json.object()

  request.new()
  |> request.set_body(req)
  |> request.set_port(3000)
  |> request.set_method(http.Post)
  |> request.set_host("http://localhost:8000/rooms/newroom")
}
