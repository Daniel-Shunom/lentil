pub type RoomStatus {
  CREATED
  DELETED
  UPDATED
  CRASHED
  STALE
}

pub type CrashReason {
  SERVERERROR
  SERVERDELAY
  SERVER
}

pub type Room {

  RoomStatus
}
