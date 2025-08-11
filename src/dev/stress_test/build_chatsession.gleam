import dev/stress_test/build_session as bd
import dev/stress_test/dev_consts as dc
import gleam/http/request
import gleam/io
import gleam/option
import gleam/otp/actor
import stratus

pub fn build_chatsession(user: bd.DevSession) {
  let path = dc.endpoint_chatroom <> user.roomid <> "/" <> user.userid
  request.new()
  |> request.set_host(dc.host)
  |> request.set_path(path)
  |> stratus.websocket(wsinit, wsloop)
  |> stratus.on_close(wsonclose)
  |> stratus.initialize()
}

fn wsinit() {
  #(Nil, option.None)
}

fn wsloop(message: stratus.Message(Nil), state, conn) {
  case message {
    stratus.Text(msg) -> {
      let _ = stratus.send_text_message(conn, "message")
      io.println("Incoming: " <> msg)
      actor.continue(state)
    }
    _ -> actor.continue(state)
  }
}

fn wsonclose(_) {
  io.println("done")
}
