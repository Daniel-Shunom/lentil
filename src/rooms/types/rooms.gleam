import global/gtypes.{type TimeStamp}
import users/types/users.{type User}

pub const small_cap = 128

pub const medium_cap = 256

pub const large_cap = 512

pub type CrashReason {
  SERVERERROR
  SERVERDELAY
}

pub type RoomCreateError {
  EXCEEDSCapacity
  NAMETaken
  CREATEFAIL
}

pub type UpdateReason {
  ADDRRoomMember
  REMOVERoomMember
  CHANGERoomOwner
  CHANGERoomCapacity
  CHANGERoomName
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
  PENDING
  CREATED(time: TimeStamp)
  DELETED(time: TimeStamp)
  UPDATED(time: TimeStamp, reason: UpdateReason)
  CRASHED(time: TimeStamp, reason: CrashReason)
}

pub type Room {
  Room(
    room_owner: User,
    room_members: List(User),
    room_status: RoomStatus,
    room_capacity: Int,
    room_id: RoomId,
    room_name: String,
  )
}
