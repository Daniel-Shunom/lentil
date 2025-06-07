import gleam/dict
import rooms/types/rooms

pub type Context {
  Context(connection_registry: dict.Dict(rooms.RoomId, String))
}
