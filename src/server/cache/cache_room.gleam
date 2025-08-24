//// This module caches room metadata via an in memory data 
//// store, and is the default caching options here. If performance 
//// issues arise, please check out other caching options in the configs.

import gleam/dict
import gleam/erlang/process
import gleam/otp/actor
import models/rooms/types/rooms

pub opaque type RoomCacheMessage {
  CacheRoom(room_info: rooms.Room)
}

type RoomKVStore {
  RoomKVStore(store: dict.Dict(String, rooms.Room))
}

pub fn cache_room(
  cache_subject subj: process.Subject(RoomCacheMessage),
  room room: rooms.Room,
) -> Nil {
  actor.send(subj, CacheRoom(room))
}

pub fn start_room_cache() -> process.Subject(RoomCacheMessage) {
  let state = RoomKVStore(dict.new())
  let assert Ok(room_cache_subj) = actor.start(state, handler)
  room_cache_subj
}

fn handler(
  message: RoomCacheMessage,
  state: RoomKVStore,
) -> actor.Next(RoomCacheMessage, RoomKVStore) {
  let CacheRoom(room) = message
  actor.continue(
    dict.insert(state.store, room.room_id.id, room)
    |> RoomKVStore,
  )
}
