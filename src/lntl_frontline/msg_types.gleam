import gleam/dict
import gleam/erlang/process
import global/gtypes
import users/types/users

pub type AuthEvent {
  SIGNIN
  SIGNUP
  SIGNOUT
}

pub type RoomEvent {
  JOINROOM
  LEAVEROOM
  JOINROOMSESSION
  LEAVEROOMSESSION
}

pub type FetchEvent {
  USER
  CLIENT
}

pub type ClientRouterMessage(msg) {
  CLIENTAuthEvent(
    userid: String,
    event_type: AuthEvent,
    success: Bool,
    lntl_time: gtypes.LentilTimeStamp,
  )
  CLIENTMessageEvent(
    userid: String,
    roomid: String,
    authenticated: Bool,
    message: msg,
  )
  CLIENTRoomEvent(
    userid: String,
    roomid: String,
    authenticated: Bool,
    event_type: RoomEvent,
    lntl_time: gtypes.LentilTimeStamp,
  )
  CLIENTFetchResource(
    userid: String,
    authenticated: Bool,
    resource_type: FetchEvent,
    lntl_time: gtypes.LentilTimeStamp,
  )
  CLIENTTokenIssued(
    userid: String,
    method: String,
    lntl_time: gtypes.LentilTimeStamp,
  )
  CLIENTTokenRevoked(
    userid: String,
    reason: String,
    lntl_time: gtypes.LentilTimeStamp,
  )
  CLIENTProtocolViolation(
    userid: String,
    reason: String,
    lntl_time: gtypes.LentilTimeStamp,
  )
  CLIENTMalformedMessage(
    userid: String,
    raw: String,
    lntl_time: gtypes.LentilTimeStamp,
  )
  CLIENTLatencyReport(
    userid: String,
    ping_ms: Int,
    lntl_time: gtypes.LentilTimeStamp,
  )
}

pub type MessageChannelStatus {
  ACTIVE
  INACTIVE
  STALE
}

pub type ServerRouterMessage(msg) {
  SERVERRoomState(
    roomid: String,
    roomstatus: String,
    members: List(users.UserId),
    activemembers: List(users.UserId),
    message_channel_status: MessageChannelStatus,
  )
  SERVERUserThrottled(userid: String, reason: String)
  SERVERUserBlocked(userid: String, reason: String)
  SERVERNodeLoad(cpu: Float, mem: Float, timestamp: gtypes.LentilTimeStamp)
  SERVERProcessCrash(process_id: String, reason: String)
}

pub type AbuseAction {
  Warn(String)
  Throttle(String)
  Block(String)
}

// NOTE -> probably needs to create a central location for the 
//         state variables.

// CLIENT STATE
pub type ClientState

pub type ClientRegistry {
  CentralClientRegistry(
    sessionid: String,
    user_logs: dict.Dict(users.UserId, ClientState),
  )
}

pub type CentralClientState {
  CentralClientState(
    // register
    dict.Dict(users.UserId, ClientRegistry),
  )
}

// SERVER STATE
pub type ServerState

pub type ServerRegistry

pub type CentralServerState {
  CentralServerState(
    // register based on significant event times
    dict.Dict(gtypes.LentilTimeStamp, ServerRegistry),
  )
}

// global monitor message
pub type GlobalMonitorMessage(message_type) {
  ClientRouterMessage(ClientRouterMessage(message_type))
  ServerRouterMessage(ServerRouterMessage(message_type))
}

pub type GlobalMonitorState(msg) {
  GlobalMonitorState(
    server: process.Subject(ServerRouterMessage(msg)),
    client: process.Subject(ClientRouterMessage(msg)),
  )
}
