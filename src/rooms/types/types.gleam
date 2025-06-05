import global_types/gtypes.{type TimeStamp}
import users/types/users.{type User}

pub const small = 128

pub const medium = 256

pub const large = 512

pub fn set_capacity(capacity: RoomCapacity) -> Int {
  case capacity {
    SMALL -> small
    MEDIUM -> medium
    LARGE -> large
  }
}

pub type CrashReason {
  SERVERERROR
  SERVERDELAY
}

pub type UpdateReason {
  ADDUser
  REMOVEUser
  CHANGEOwner
  CHANGECapacity
  CHANGEName
}

pub type RoomCapacity {
  SMALL
  MEDIUM
  LARGE
}

pub type RoomId {
  RoomId(id: String, created: TimeStamp)
}

pub type RoomStatus {
  CREATED(time: TimeStamp)
  DELETED(time: TimeStamp)
  UPDATED(time: TimeStamp, reason: UpdateReason)
  CRASHED(time: TimeStamp, reason: CrashReason)
}

pub type Room {
  Room(
    owner: User,
    members: List(User),
    status: RoomStatus,
    capacity: RoomCapacity,
    room_id: RoomId,
    room_name: String,
  )
}
