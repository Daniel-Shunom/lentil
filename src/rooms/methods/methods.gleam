import gleam/int
import gleam/list
import global/functions.{get_timestamp}
import lntl_server/sql
import pog
import prng/random
import prng/seed
import rooms/types/rooms
import users/types/users

pub fn create_room(
  room_owner owner: users.UserId,
  room_name name: String,
  room_members members: List(users.UserId),
  room_capacity capacity: rooms.RoomCapacity,
  connection conn: pog.Connection,
) -> Result(rooms.Room, rooms.RoomCreateError) {
  let is_cap = set_capacity(capacity) - 1 >= list.length(members)
  let name_exists = check_name(name, conn)
  case is_cap {
    False -> Error(rooms.EXCEEDSCapacity)
    True -> {
      case name_exists {
        True -> Error(rooms.NAMETaken)
        False -> {
          let create_time = get_timestamp()
          // TODO -> add verification logic to ensure room creation.
          Ok(
            rooms.Room(
              room_owner: owner,
              room_members: [owner, ..members],
              room_status: rooms.CREATED(time: create_time),
              room_capacity: set_capacity(capacity),
              room_id: rooms.RoomId(id: generate_room_id()),
              room_name: name,
              room_announcements: [],
            ),
          )
        }
      }
    }
  }
}

pub fn get_room_owner(room: rooms.Room) -> users.UserId {
  room.room_owner
}

pub fn get_room_members(room: rooms.Room) -> List(users.UserId) {
  room.room_members
}

pub fn get_room_status(room: rooms.Room) -> rooms.RoomStatus {
  room.room_status
}

pub fn get_room_name(room: rooms.Room) -> String {
  room.room_name
}

pub fn change_room_name(new_name: String, room: rooms.Room) -> rooms.Room {
  let new_time = get_timestamp()
  let new_reason = rooms.CHANGERoomName
  rooms.Room(
    ..room,
    room_name: new_name,
    room_status: rooms.UPDATED(time: new_time, reason: new_reason),
  )
}

pub fn change_room_owner(
  new_owner: users.UserId,
  room: rooms.Room,
) -> rooms.Room {
  let new_time = get_timestamp()
  let new_reason = rooms.CHANGERoomOwner
  rooms.Room(
    ..room,
    room_owner: new_owner,
    room_status: rooms.UPDATED(time: new_time, reason: new_reason),
  )
}

pub fn change_room_status(
  new_status: rooms.RoomStatus,
  room: rooms.Room,
) -> rooms.Room {
  rooms.Room(..room, room_status: new_status)
}

pub fn change_room_capacity(
  new_capacity: rooms.RoomCapacity,
  room: rooms.Room,
) -> rooms.Room {
  let new_time = get_timestamp()
  let new_reason = rooms.CHANGERoomCapacity
  rooms.Room(
    ..room,
    room_capacity: set_capacity(new_capacity),
    room_status: rooms.UPDATED(time: new_time, reason: new_reason),
  )
}

pub fn add_room_member(
  room: rooms.Room,
  user: users.UserId,
) -> Result(rooms.Room, rooms.RoomCreateError) {
  let num_members = list.length(room.room_members)
  case room.room_capacity > num_members {
    False -> Error(rooms.EXCEEDSCapacity)
    True -> {
      case list.any(room.room_members, fn(mem) { user == mem }) {
        True -> Ok(room)
        False -> {
          let new_members = list.prepend(room.room_members, user)
          let new_time = get_timestamp()
          let new_reason = rooms.ADDRRoomMember
          Ok(
            rooms.Room(
              ..room,
              room_members: new_members,
              room_status: rooms.UPDATED(time: new_time, reason: new_reason),
            ),
          )
        }
      }
    }
  }
}

pub fn remove_room_member(room: rooms.Room, user: users.UserId) -> rooms.Room {
  let pruned = list.filter(room.room_members, fn(member) { member != user })
  let new_time = get_timestamp()
  rooms.Room(
    ..room,
    room_members: pruned,
    room_status: rooms.UPDATED(time: new_time, reason: rooms.REMOVERoomMember),
  )
}

pub fn set_capacity(capacity: rooms.RoomCapacity) -> Int {
  case capacity {
    rooms.SMALL -> rooms.small_cap
    rooms.MEDIUM -> rooms.medium_cap
    rooms.LARGE -> rooms.large_cap
  }
}

fn generate_room_id() -> String {
  let str = random.fixed_size_string(32)
  let num = random.int(0, 100_000)
  let secure_prefix =
    random.int(0, 13)
    |> random.random_sample()
    |> seed.new()
    |> random.sample(num, _)
    |> int.to_string()

  let secure_id =
    random.int(0, 9)
    |> random.random_sample()
    |> seed.new()
    |> random.sample(str, _)

  "lntl-rm-" <> secure_prefix <> secure_id
}

fn check_name(name: String, conn: pog.Connection) -> Bool {
  case sql.fetch_name_exists(conn, name) {
    Error(_) -> False
    Ok(res) -> {
      list.any(res.rows, fn(x) { x.available == True })
    }
  }
}
