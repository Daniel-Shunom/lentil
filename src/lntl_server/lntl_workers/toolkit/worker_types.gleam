import gleam/erlang/process.{type Pid, type Subject}
import gleam/option.{type Option}
import gleam/set.{type Set}
import gleam/time/duration.{type Duration}
import messages/types/msg.{type Message, type MessageQueue}
import rooms/types/rooms.{type Room, type RoomCapacity, type RoomId}
import users/types/users.{type User}

pub type RoomSession {
  RoomSession(
    room_data: Room,
    session_id: String,
    retry_bin: List(Message),
    connection_registry: Set(Pid),
  )
}

pub type UserSession {
  UserSession(
    session_id: String,
    user: User,
    queue: MessageQueue,
    member_rooms: List(RoomId),
    owned_rooms: List(RoomId),
    task_inbox: Subject(SessionOperationMessage),
  )
}

// This type indicates the type of messages a room session
// can have.
pub type RoomSessionMessage {
  DELETEROOM(User, Subject(SessionOperationMessage))
  SENDMESSAGE(Message, Subject(SessionOperationMessage))
  CONNECT(User, Pid, Subject(SessionOperationMessage))
  DISCONNECT(User, Pid, Subject(SessionOperationMessage))
  // when the user want's to just leave the room themselves.
  LEAVE(User, Pid, Subject(SessionOperationMessage))
  // when the user want's to join the room and become a member.
  JOIN(User, Pid, Subject(SessionOperationMessage))

  // these variants can only be used by the owner.
  // the handler will check that the person sending
  // this message is the owner
  UPDATENAME(User, String, Subject(SessionOperationMessage))
  UPDATECAPACITY(User, RoomCapacity, Subject(SessionOperationMessage))
  ANNOUNCE(User, String, Subject(SessionOperationMessage))
  REMOVEMEMBER(User, Subject(SessionOperationMessage))
  MEMBERTIMEOUT(User, Duration, Subject(SessionOperationMessage))

  // this will be used in case of some malicious
  // activity that can only be cleared by shutting
  // down the process.
  PING(Pid)
  SHUTDOWN
}

pub type UserSessionMessage {
  // to create a new room
  CREATEROOM
  // to delete an owned room
  DELETE(RoomId)
  // to join an existing room
  JOINROOM(RoomId)
  // to leave an existing room
  LEAVEROOM(RoomId)
  // to connect to a room session
  ROOMCONNECT(RoomId)
  // to disconenct from a room session
  ROOMDISCONNECT(RoomId)
  // to send a message while in a room session
  MESSAGEROOM(Message, RoomId)
  // to make an announcement to a room you own
  ROOMANOUNCEMENT(Message, RoomId)
  // to remove a member from the owners room
  KICKOFFMEMBER(User, RoomId)
  // to timeout a member from sending messages
  TIMEOUTMEMBER(User, RoomId, Duration)
}

pub type SessionOperationMessage {
  SUCCESS(String)
  FAILURE(String)
}
