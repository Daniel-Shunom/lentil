import gleam/list
import global/functions.{get_timestamp}
import rooms/types/rooms
import users/types/users

pub fn create_room(
  room_owner owner: users.User,
  room_name name: String,
  room_members members: List(users.User),
  room_capacity capacity: rooms.RoomCapacity,
) -> Result(rooms.Room, rooms.RoomCreateError) {
  let is_cap = set_capacity(capacity) - 1 >= list.length(members)
  let name_exists = check_name(name)
  case is_cap {
    False -> Error(rooms.EXCEEDSCapacity)
    True -> {
      case name_exists {
        True -> Error(rooms.NAMETaken)
        False -> {
          let create_time = get_timestamp()
          // TODO -> add verification logic to ensure room creation.
          Ok(rooms.Room(
            room_owner: owner,
            room_members: [owner, ..members],
            room_status: rooms.CREATED(time: create_time),
            room_capacity: set_capacity(capacity),
            room_id: rooms.RoomId(id: generate_room_id(), created: create_time),
            room_name: name,
          ))
        }
      }
    }
  }
}

pub fn get_room_owner(room: rooms.Room) -> users.User {
  room.room_owner
}

pub fn get_room_members(room: rooms.Room) -> List(users.User) {
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

pub fn change_room_owner(new_owner: users.User, room: rooms.Room) -> rooms.Room {
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
  user: users.User,
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

pub fn remove_room_member(room: rooms.Room, user: users.User) -> rooms.Room {
  let pruned = list.filter(room.room_members, fn(member) { member != user })
  let new_time = get_timestamp()
  rooms.Room(
    ..room,
    room_members: pruned,
    room_status: rooms.UPDATED(time: new_time, reason: rooms.REMOVERoomMember),
  )
}

fn set_capacity(capacity: rooms.RoomCapacity) -> Int {
  case capacity {
    rooms.SMALL -> rooms.small_cap
    rooms.MEDIUM -> rooms.medium_cap
    rooms.LARGE -> rooms.large_cap
  }
}

fn generate_room_id() -> String {
  "lntl-" <> ""
}

fn check_name(name: String) -> Bool {
  todo
}
