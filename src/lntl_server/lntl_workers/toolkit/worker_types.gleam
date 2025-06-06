import gleam/erlang/process
import gleam/set
import gleam/time/duration

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
    member_rooms: List(rooms.RoomId),
    owned_rooms: List(rooms.RoomId),
    task_inbox: process.Subject(SessionOperationMessage),
  )
}

// This type indicates the type of messages a room session
// can have.
pub type RoomSessionMessage {
  DELETEROOM(users.User, process.Subject(SessionOperationMessage))
  SENDMESSAGE(msg.Message, process.Subject(SessionOperationMessage))
  CONNECT(users.User, process.Pid, process.Subject(SessionOperationMessage))
  DISCONNECT(users.User, process.Pid, process.Subject(SessionOperationMessage))
  LEAVE(users.User, process.Pid, process.Subject(SessionOperationMessage))
  JOIN(users.User, process.Pid, process.Subject(SessionOperationMessage))
  UPDATENAME(users.User, String, process.Subject(SessionOperationMessage))
  UPDATECAPACITY(
    users.User,
    rooms.RoomCapacity,
    process.Subject(SessionOperationMessage),
  )
  ANNOUNCE(users.User, String, process.Subject(SessionOperationMessage))
  REMOVEMEMBER(users.User, process.Subject(SessionOperationMessage))
  MEMBERTIMEOUT(
    users.User,
    duration.Duration,
    process.Subject(SessionOperationMessage),
  )
  PING(process.Pid)
  SHUTDOWN
}

pub type UserSessionMessage {
  // to create a new room
  CREATEROOM
  // to delete an owned room
  DELETE(rooms.RoomId)
  // to join an existing room
  JOINROOM(rooms.RoomId)
  // to leave an existing room
  LEAVEROOM(rooms.RoomId)
  // to connect to a room session
  ROOMCONNECT(rooms.RoomId)
  // to disconnect from a room session
  ROOMDISCONNECT(rooms.RoomId)
  // to send a message while in a room session
  MESSAGEROOM(msg.Message, rooms.RoomId)
  // to make an announcement to a room you own
  ROOMANNOUNCEMENT(msg.Message, rooms.RoomId)
  // to remove a member from the owner’s room
  KICKOFFMEMBER(users.User, rooms.RoomId)
  // to timeout a member from sending messages
  TIMEOUTMEMBER(users.User, rooms.RoomId, duration.Duration)
}

pub type SessionOperationMessage {
  SUCCESS(String)
  FAILURE(String)
}
