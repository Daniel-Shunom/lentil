import gleam/erlang/process
import global/gtypes

pub type ConnectionMonitorMessage {
  UserConnected(
    userid: String,
    sessionid: String,
    timestamp: gtypes.LentilTimeStamp,
  )
  UserDisconnected(
    userid: String,
    sessionid: String,
    timestamp: gtypes.LentilTimeStamp,
  )
  Heartbeat(
    userid: String,
    sessionid: String,
    ping_ms: Int,
    timestamp: gtypes.LentilTimeStamp,
  )
  GetActiveUsers(process.Subject(List(String)))
  // async query
  // GetUserSessionInfo(userid: String, process.Subject(ClientState)) // optional
}
