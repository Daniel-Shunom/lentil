import models/rooms/types/rooms
import models/users/types/users
import models/messages/types/msg
import gleam/set
import gleam/erlang/process
import gleam/dict

/// A room session is an on-server representation of an ideal
/// long running room process and its state.
/// 
/// A room session lives for as long as the server is 
/// active or/and as long as the room is alive
pub type RoomSession {
  RoomSession(
    // room information
    room_data: rooms.Room,
    // current session-id. This is created once the server is
    // alive and upon the creation of a new room.
    session_id: String,
    // a bucket for containing message retries.
    retry_bin: List(msg.Message),
    // set of active connected users and their on-server PIDs
    connection_registry: set.Set(process.Pid),
    // set of subscribed users and their WebSocket adapter subjects.
    // this is for forwarding incoming messages to all connected
    // client adapters
    broadcast_pool: set.Set(process.Subject(RoomMessageStream)),
  )
}

pub type UserSession {
  UserSession(
    // current user's session id
    session_id: String,
    // user in-server representation
    user: users.User,
    // current message backlog
    queue: msg.MessageQueue,
    // rooms of which the user is a member, and the associated room-id
    member_rooms: dict.Dict(rooms.RoomId, process.Subject(RoomSessionMessage)),
    // Adapter for communicating with websocket channel
    task_inbox: process.Subject(RoomMessageStream),
    // list of owned rooms, if any.
    owned_rooms: List(rooms.RoomId),
  )
}

/// A message type for between a room, and a user's WebSocket subject.
/// 
/// User websockets use this message type for subscribing to a room's 
/// message pool via its adapter. 
pub type RoomMessageStream {
  // Unsubscribe a WebSocket subject from recieving room adapter subject.
  UnsubscribeWS
  // Subscribe a WebSocket subject to an adapter for recieving messages.
  SubscribeWS(mailbox: process.Subject(RoomMessageStream))
  // Incoming essage from adapter to WebSocket subject.
  Incoming(roomid: String, message: msg.Message)
}


/// A room is a long running process with its own suejct. Thie type describes
/// the kind of message a Room process can receive on the server.
/// 
/// In the future when we want to update the current features of a room, we can
/// add more message variants for interacting with that feature from other 
/// subjects.
pub type RoomSessionMessage {
  // Allows permanent shutdown of a room process and its consequent deletion
  DeleteRoom(users.UserId, process.Subject(UserSessionMessage))
  // For receiving incoming messages from user processes.
  ReceiveMessageFromUser(msg.Message, process.Subject(RoomMessageStream))
  // For sending messages to a user WebSocket message adapter
  SendMessageFromRoom(msg.Message, process.Subject(RoomMessageStream))
  // Connect a user to an active room session.
  ConnectUser(
    userid: users.UserId,
    process_id: process.Pid,
    user_session_process: process.Subject(UserSessionMessage),
    user_mailbox_process: process.Subject(RoomMessageStream),
  )
  // Disconnect a user from an active room session
  DisconnectUser(
    userid: users.UserId,
    user_process: process.Pid,
    user_process_subject: process.Subject(UserSessionMessage),
    user_mailbox_process: process.Subject(RoomMessageStream),
  )
  // Allows the user to leave the room permanently. 
  LeaveRoom(users.UserId, process.Pid, process.Subject(UserSessionMessage))
  // Allows the message to join a room as a member
  JoinRoom(users.UserId, process.Pid, process.Subject(UserSessionMessage))
  // Change the name of an active room.
  UpdateRoomName(users.UserId, String, process.Subject(UserSessionMessage))
  // Change the capacity of an active room.
  UpdateRoomCapacity(
    users.UserId,
    rooms.RoomCapacity,
    process.Subject(UserSessionMessage),
  )
  // Announce something to an active room.
  AnnounceToRoom(
    users.UserId,
    rooms.RoomAnouncement,
    process.Subject(UserSessionMessage),
  )
  // Remove a member from an active room.
  RemoveFromRoom(users.UserId, process.Subject(UserSessionMessage))
  // Ping a connected member of a room. Will be used to trim
  // stale/dead coonnections.
  Ping(process.Pid, process.Subject(UserSessionMessage))
  // Shutdown an active room
  // This will be intiated by the server for probably maintenance purposes.
  ShutdownRoom
}


pub type UserSessionMessage {
  /// Register that we've got a live room process we can talk to
  AddToRoom(
    room_id: rooms.RoomId,
    room_subj: process.Subject(RoomSessionMessage),
  )
  /// Send a chat into a roomof which the user is a member.
  SendToRoom(room_id: rooms.RoomId, text: msg.Message)
  // Upon a sucessull delivery of a message to a room, the user session 
  // will receive this mesage type.
  MessageDelivered(msg.Message)
  // a basic success message
  Success(String)
  // a basic failure message
  Failure(String)
  // To shutdown a user session. 
  ShutdownUserSession
}
