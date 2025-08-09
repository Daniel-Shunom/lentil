import gleam/httpc
import gleam/http
import gleam/http/request
import gleam/json
import gleam/list
import dev/stress_test/dev_consts as dc
import gleam/dynamic/decode

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
    |> json.to_string()

  request.new()
  |> request.set_host(dc.host)
  |> request.set_path(dc.endpoint_newroom)
  |> request.set_body(req)
  |> request.set_method(http.Post)
  |> httpc.send()
}

pub fn dev_roomdecoder() {
  use roomid <- decode.field("roomid", decode.string)
  use roomname <- decode.field("roomname", decode.string)
  decode.success(DevRoom(roomid, roomname))
}

pub type DevRoom{
  DevRoom(id: String, name: String)
}


