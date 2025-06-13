import gleam/dict
import gleam/erlang/process.{type Subject}
import lntl_server/lntl_workers/toolkit/worker_types as wt
import rooms/types/rooms
import users/types/users.{type User, type UserId}

pub type RmSupMsg {
  DELROOM(sessionid: String)
  NEWROOM(
    userid: UserId,
    capacity: rooms.RoomCapacity,
    roomname: String,
    roomid: String,
  )
}

pub type SupMsg {
  ADD(User)
  REM(String)
  MSG(userid: UserId, roomid: String, message: String)
}

pub type RmMsg {
  NEW(sessionid: String, sessionmailbox: Subject(wt.RoomSessionMessage))
  DEL(sessionid: String)
  SEND(sessionid: String, message: wt.RoomSessionMessage)
}

pub type CtxMsg {
  AddToCtx(userid: String, usersubj: Subject(wt.SessionOperationMessage))
  MsgToUserProc(userid: String, roomid: String, roommessage: String)
  DelFrmCtx(userid: String)
}

pub type RmSupState {
  RmSupState(room_supervisorbox: Subject(RmSupMsg), context: Subject(RmMsg))
}

pub type CtxState {
  CtxState(registry: dict.Dict(String, Subject(wt.SessionOperationMessage)))
}

pub type SupState {
  SupState(
    supbox: Subject(SupMsg),
    ctx: Subject(CtxMsg),
    roomsupbox: Subject(RmMsg),
  )
}

pub type RmState {
  RmState(registry: dict.Dict(String, Subject(wt.RoomSessionMessage)))
}
