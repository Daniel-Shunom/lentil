import gleam/function
import gleam/erlang/process
import gleam/option.{type Option, Some}
import gleam/otp/actor
import mist
import lntl_frontline/msg_types as mt
// import wisp

pub fn stream_serverside_events(req, monitor) {
  mist.websocket(req, wshandler, on_init(_, monitor), on_close)
}

fn wshandler(
  state: Nil,
  conn: mist.WebsocketConnection,
  message: mist.WebsocketMessage(String),
) -> actor.Next(String, Nil) {
  case message {
    mist.Closed | mist.Shutdown -> actor.Stop(process.Normal)
    mist.Text(_text) -> actor.continue(state)
    mist.Custom(custom) -> {
      let _ = mist.send_text_frame(conn, custom)
      actor.continue(state)
    }
    _ -> actor.continue(state)
  }
}

fn on_init(
  conn: mist.WebsocketConnection,
  monitor: process.Subject(mt.GlobalMonitorMessage(a))
) -> #(Nil, Option(process.Selector(String))) {
  let ws_subj = process.new_subject()
  let _ = mist.send_text_frame(conn, "WELCOME TO YOUR LENTIL DASHBOARD")
  mt.StreamChannelSubject(ws_subj)
  |> actor.send(monitor, _)

  let reciever = 
    process.new_selector()
    |> process.selecting(ws_subj, function.identity)
  #(Nil, Some(reciever))
}

fn on_close(_state: Nil) -> Nil {
  Nil
}
