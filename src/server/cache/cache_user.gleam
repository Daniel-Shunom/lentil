//// This module caches all logged in users via an in memory data 
//// store, and is the default caching options here. If performance 
//// issues arise, please check out other caching options in the configs.

import gleam/dict
import gleam/erlang/process
import gleam/otp/actor
import models/users/types/users

pub opaque type UserCacheMessage {
  CacheUser(user_info: users.User)
  UnCacheUser(userid: String)
  FlushCache
}

type UserKVStore {
  UserKVStore(store: dict.Dict(String, users.User))
}

pub fn cache_user(
  cache_subject subj: process.Subject(UserCacheMessage),
  user user: users.User,
) -> Nil {
  actor.send(subj, CacheUser(user))
}

pub fn uncache_user(
  cache_subject subj: process.Subject(UserCacheMessage),
  userid userid: String,
) -> Nil {
  actor.send(subj, UnCacheUser(userid))
}

pub fn flush_user_cache(
  cache_subject subj: process.Subject(UserCacheMessage),
) -> Nil {
  actor.send(subj, FlushCache)
}

pub fn start_user_cache() -> process.Subject(UserCacheMessage) {
  let state = UserKVStore(dict.new())
  let assert Ok(cache_subject) = actor.start(state, handler)
  cache_subject
}

fn handler(
  messsage: UserCacheMessage,
  state: UserKVStore,
) -> actor.Next(UserCacheMessage, UserKVStore) {
  case messsage {
    CacheUser(user) ->
      actor.continue(
        dict.insert(state.store, user.user_id.id, user)
        |> UserKVStore,
      )

    UnCacheUser(userid) ->
      actor.continue(
        dict.delete(state.store, userid)
        |> UserKVStore,
      )

    FlushCache ->
      actor.continue(
        dict.new()
        |> UserKVStore,
      )
  }
}
