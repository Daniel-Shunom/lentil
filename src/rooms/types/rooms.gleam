import global/gtypes.{type LentilTimeStamp}
import users/types/users.{type User, type UserId}

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
  RoomId(id: String, created: LentilTimeStamp)
}

pub type RoomAnouncement {
  RoomAnouncement(message: String, time: LentilTimeStamp)
}

pub type RoomStatus {
  PENDING
  CREATED(time: LentilTimeStamp)
  DELETED(time: LentilTimeStamp)
  UPDATED(time: LentilTimeStamp, reason: UpdateReason)
  CRASHED(time: LentilTimeStamp, reason: CrashReason)
}

pub type Room {
  Room(
    room_owner: UserId,
    room_members: List(UserId),
    room_status: RoomStatus,
    room_capacity: Int,
    room_id: RoomId,
    room_name: String,
    room_announcements: List(RoomAnouncement),
  )
}
