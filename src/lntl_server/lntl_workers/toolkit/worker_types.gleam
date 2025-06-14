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
    connection_registry: set.Set(process.Pid),
  )
}

pub type UserSession {
  UserSession(
    session_id: String,
    user: users.User,
    queue: msg.MessageQueue,
    member_rooms: dict.Dict(rooms.RoomId, process.Subject(RoomSessionMessage)),
    task_inbox: process.Subject(SessionOperationMessage),
    owned_rooms: List(rooms.RoomId),
  )
}

// This type indicates the type of messages a room session
// can have.
pub type RoomSessionMessage {
  DELETEROOM(users.UserId, process.Subject(SessionOperationMessage))
  SENDMESSAGE(msg.Message, process.Subject(SessionOperationMessage))
  CONNECT(users.UserId, process.Pid, process.Subject(SessionOperationMessage))
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
