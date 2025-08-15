import server/cache/cache_room
import gleam/bool
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import global/ctx/ctx
import global/functions.{id_generator}
import models/rooms/methods/methods.{create_room}
import models/rooms/types/rooms.{type RoomCapacity}
import models/users/types/users.{UserId}
import pog
import server/sql
import wisp

pub fn handle_create_room(req: wisp.Request, ctx: ctx.Context) -> wisp.Response {
  use <- wisp.require_content_type(req, "application/json")
  use json <- wisp.require_json(req)
  let bar =
    {
      use RoomReqs(roomid, ownerid, roomname, capacity, status) <- result.try(
        decode.run(json, room_decoder())
        |> result.replace_error(wisp.response(400)),
      )

      use pog.Returned(_, rows) <- result.try(
        sql.create_new_room(
          ctx.db_connection,
          roomid,
          ownerid,
          roomname,
          get_cap(capacity),
          status,
        )
        |> result.replace_error(wisp.response(400)),
      )

      use <- bool.guard(list.is_empty(rows), Error(wisp.response(400)))

      use room <- result.try(
        create_room(
          room_owner: UserId(ownerid),
          room_name: roomname,
          room_members: [],
          room_capacity: capacity,
          connection: ctx.db_connection,
        )
        |> result.replace_error(wisp.response(400)),
      )

      cache_room.cache_room(ctx.room_cache, room)
      let roomid = #("roomid", json.string(roomid))
      let roomname = #("roomname", json.string(roomname))

      list.new()
      |> list.prepend(roomid)
      |> list.prepend(roomname)
      |> json.object()
      |> json.to_string_tree()
      |> wisp.json_response(200)
      |> Ok
    }
  result.unwrap_both(bar)
}

fn room_decoder() -> decode.Decoder(RoomReqs) {
  use ownerid <- decode.field("userid", decode.string)
  use roomname <- decode.field("roomname", decode.string)
  use cap <- decode.field("capacity", decode.string)
  decode.success(RoomReqs(
    roomid: id_generator("lntl-rm"),
    owner_id: ownerid,
    roomname: roomname,
    capacity: capacity(cap),
    status: "",
  ))
}

type RoomReqs {
  RoomReqs(
    roomid: String,
    owner_id: String,
    roomname: String,
    capacity: RoomCapacity,
    status: String,
  )
}

fn capacity(str: String) -> RoomCapacity {
  let capacity =
    string.trim(str)
    |> string.lowercase()
  case capacity {
    "small" -> rooms.SMALL
    "medium" -> rooms.MEDIUM
    "large" -> rooms.LARGE
    _ -> rooms.SMALL
  }
}

fn get_cap(cap: RoomCapacity) -> Int {
  case cap {
    rooms.SMALL -> rooms.small_cap
    rooms.MEDIUM -> rooms.medium_cap
    rooms.LARGE -> rooms.large_cap
  }
}
