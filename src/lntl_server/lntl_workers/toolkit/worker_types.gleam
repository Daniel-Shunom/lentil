import gleam/dict
import gleam/erlang/process
import gleam/set
import messages/types/msg
import rooms/types/rooms
import users/types/users

pub type SESSIONTYPE {
  ROOMSESSION
  USERSESSION
}

pub type RoomSession {
  RoomSession(
    room_data: rooms.Room,
    session_id: String,
    retry_bin: List(msg.Message),
    // pid's for user active actors containing the 
    // SessionOperationMessage message type
    connection_registry: set.Set(process.Pid),
    broadcast_pool: set.Set(process.Subject(RoomMessageStream)),
    // bin_handler: process.Subject(Retry)
  )
}

pub type BinState {
  BinState(
    bin: List(msg.Message),
    session_subject: process.Subject(RoomSessionMessage),
    sender_subject: process.Subject(SessionOperationMessage),
    sender_stream_box: process.Subject(RoomMessageStream),
  )
}

pub type Retry {
  Retry(msg.Message)
}

pub type UserSession {
  UserSession(
    session_id: String,
    user: users.User,
    queue: msg.MessageQueue,
    member_rooms: dict.Dict(rooms.RoomId, process.Subject(RoomSessionMessage)),
    task_inbox: process.Subject(RoomMessageStream),
    owned_rooms: List(rooms.RoomId),
  )
}

// This type indicates the type of messages a room session
// can have.
pub type RoomMessageStream {
  SUBSCRIBEWS(mailbox: process.Subject(RoomMessageStream))
  INCOMING(roomid: String, message: msg.Message)
}

pub type ForwardToWs(msg) {
  ForwardToWs(roomid: String, message: msg)
}

pub type RoomSessionMessage {
  DELETEROOM(users.UserId, process.Subject(SessionOperationMessage))
  SENDMESSAGE(msg.Message, process.Subject(RoomMessageStream))
  CONNECT(
    userid: users.UserId,
    process_id: process.Pid,
    user_session_process: process.Subject(SessionOperationMessage),
    user_mailbox_process: process.Subject(RoomMessageStream),
  )
  DISCONNECT(
    users.UserId,
    process.Pid,
    process.Subject(SessionOperationMessage),
  )
  LEAVE(users.UserId, process.Pid, process.Subject(SessionOperationMessage))
  JOIN(users.UserId, process.Pid, process.Subject(SessionOperationMessage))
  UPDATENAME(users.UserId, String, process.Subject(SessionOperationMessage))
  UPDATECAPACITY(
    users.UserId,
    rooms.RoomCapacity,
    process.Subject(SessionOperationMessage),
  )
  ANNOUNCE(
    users.UserId,
    rooms.RoomAnouncement,
    process.Subject(SessionOperationMessage),
  )
  REMOVEMEMBER(users.UserId, process.Subject(SessionOperationMessage))
  // TODO -> fleshout this functionality later
  //MEMBERTIMEOUT(
  //  users.UserId,
  //  duration.Duration,
  //  process.Subject(SessionOperationMessage),
  //)
  PING(process.Pid, process.Subject(SessionOperationMessage))
  SHUTDOWN
}

pub type SessionOperationMessage {
  /// Register that we've got a live room process we can talk to
  ADDROOM(room_id: rooms.RoomId, room_subj: process.Subject(RoomSessionMessage))
  /// Send a chat into a room you’ve joined
  SENDTOROOM(room_id: rooms.RoomId, text: msg.Message)
  MESSAGEDELIVERED(msg.Message)
  SUCCESS(String)
  FAILURE(String)
  CLOSESESSION
}
