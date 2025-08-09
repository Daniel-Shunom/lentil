import gleam/dict
import gleam/erlang/process.{type Subject}

// import gleam/option
import models/rooms/types/rooms
import models/users/types/users.{type User, type UserId}
import server/workers/shared/shared_types as st

pub type RmSupMsg {
  DELROOM(sessionid: String)
  ADDTOBROADCAST(
    userid: UserId,
    roomid: String,
    ws_inbox: process.Subject(st.RoomMessageStream),
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
        process.Subject(st.UserSessionMessage),
        process.Subject(st.RoomMessageStream),
      ),
    ),
  )
}

pub type RmMsg {
  NEW(roomid: String, sessionmailbox: Subject(st.RoomSessionMessage))
  DEL(roomid: String)
  SEND(roomid: String, message: st.RoomSessionMessage)
  BCT(
    roomid: String,
    userid: String,
    user_client: process.Subject(st.UserSessionMessage),
    user_mailbox: process.Subject(st.RoomMessageStream),
  )
  RMBCT(
    roomid: String,
    userid: String,
    user_client: process.Subject(st.UserSessionMessage),
    ws_inbox: process.Subject(st.RoomMessageStream),
  )
}

pub type CtxMsg {
  AddToCtx(
    userid: String,
    userhandles: #(
      Subject(st.UserSessionMessage),
      Subject(st.RoomMessageStream),
    ),
  )
  MsgToUserProc(userid: String, roomid: String, roommessage: String)
  DelFrmCtx(userid: String)
  FetchUserSession(
    userid: String,
    reply_to: process.Subject(
      #(
        process.Subject(st.UserSessionMessage),
        process.Subject(st.RoomMessageStream),
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
      #(Subject(st.UserSessionMessage), Subject(st.RoomMessageStream)),
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
  RmState(registry: dict.Dict(String, Subject(st.RoomSessionMessage)))
}
