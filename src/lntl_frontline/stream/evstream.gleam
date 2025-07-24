import gleam/erlang/process
import gleam/option.{type Option, None}
import gleam/otp/actor
import mist

// import wisp

pub fn stream_serverside_events(req) {
  mist.websocket(req, wshandler, on_init, on_close)
}

fn wshandler(
  _state: Nil,
  _conn: mist.WebsocketConnection,
  message: mist.WebsocketMessage(String),
) -> actor.Next(String, Nil) {
  case message {
    mist.Closed | mist.Shutdown -> actor.Stop(process.Normal)
    mist.Text(text) -> actor.continue(Nil)
    mist.Custom(custom) -> {
      todo
    }
    _ -> actor.continue(Nil)
  }
}

fn on_init(
  conn: mist.WebsocketConnection,
) -> #(Nil, Option(process.Selector(String))) {
  #(Nil, None)
}

fn on_close(_state: Nil) -> Nil {
  todo
}
