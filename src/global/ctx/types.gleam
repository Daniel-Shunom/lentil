import gleam/dict
import gleam/erlang/process.{type Subject}

// import gleam/option
import models/rooms/types/rooms
import models/users/types/users.{type User, type UserId}
import server/workers/toolkit/worker_types as wt

pub type RmSupMsg {
  DELROOM(sessionid: String)
  ADDTOBROADCAST(
    userid: UserId,
    roomid: String,
    ws_inbox: process.Subject(wt.RoomMessageStream),
  )
  REMOVEFROMBROADCAST(userid: UserId, roomid: String)
  NEWROOM(
    userid: UserId,
    capacity: rooms.RoomCapacity,
    roomname: String,
    roomid: String,
  )
}

pub type SupMsg {
  ADD(user: User)
  REM(userid: String)
  MSG(userid: UserId, roomid: String, message: String)
  GETUSERSESSION(
    user_id: users.UserId,
    reply_to: process.Subject(
      #(
        process.Subject(wt.SessionOperationMessage),
        process.Subject(wt.RoomMessageStream),
      ),
    ),
  )
}

pub type RmMsg {
  NEW(roomid: String, sessionmailbox: Subject(wt.RoomSessionMessage))
  DEL(roomid: String)
  SEND(roomid: String, message: wt.RoomSessionMessage)
  BCT(
    roomid: String,
    userid: String,
    user_client: process.Subject(wt.SessionOperationMessage),
    user_mailbox: process.Subject(wt.RoomMessageStream),
  )
  RMBCT(
    roomid: String,
    userid: String,
    user_client: process.Subject(wt.SessionOperationMessage),
    ws_inbox: process.Subject(wt.RoomMessageStream),
  )
}

pub type CtxMsg {
  AddToCtx(
    userid: String,
    userhandles: #(
      Subject(wt.SessionOperationMessage),
      Subject(wt.RoomMessageStream),
    ),
  )
  MsgToUserProc(userid: String, roomid: String, roommessage: String)
  DelFrmCtx(userid: String)
  FetchUserSession(
    userid: String,
    reply_to: process.Subject(
      #(
        process.Subject(wt.SessionOperationMessage),
        process.Subject(wt.RoomMessageStream),
      ),
    ),
  )
}

pub type RmSupState {
  RmSupState(room_supervisorbox: Subject(RmSupMsg), context: Subject(RmMsg))
}

pub type CtxState {
  CtxState(
    registry: dict.Dict(
      String,
      #(Subject(wt.SessionOperationMessage), Subject(wt.RoomMessageStream)),
    ),
  )
}

pub type SupState {
  SupState(
    // supbox: Subject(SupMsg),
    ctx: Subject(CtxMsg),
    roomsupbox: Subject(RmMsg),
  )
}

pub type RmState {
  // A registry of roomid's and their subjects
  RmState(registry: dict.Dict(String, Subject(wt.RoomSessionMessage)))
}
