import gleam/option
import dev/stress_test/build_session as bd
import gleam/http/request
import dev/stress_test/dev_consts as dc
import stratus
import gleam/io

pub fn build_chatsession(user: bd.DevSession) {
  let path = 
    dc.endpoint_chatroom 
    <> user.roomid 
    <> "/" 
    <> user.userid
  request.new()
  |> request.set_host(dc.host)
  |> request.set_path(path)
  |> stratus.websocket(wsinit, wsloop)
  |> stratus.on_close(wsonclose)
  |> stratus.initialize()
}

fn wsinit() { #(Nil, option.None) }
fn wsloop(state, message, conn) {
  todo
}
fn wsonclose(_) { io.println("done") }