import gleam/dict
import gleam/erlang/process.{type Subject}
import global/functions.{connect_lentildb}
import lntl_server/lntl_workers/w_users/u_session as us
import pog
import rooms/types/rooms

pub type Context {
  Context(
    connection_registry: dict.Dict(rooms.RoomId, String),
    user_session: Subject(us.UserSessionRegistryMessage),
    db_connection: pog.Connection,
  )
}

pub fn get_context() -> Context {
  Context(
    connection_registry: dict.new(),
    user_session: process.new_subject(),
    db_connection: connect_lentildb(),
  )
}
