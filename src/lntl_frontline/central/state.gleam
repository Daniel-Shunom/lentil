import gleam/dict
import gleam/erlang/process
import gleam/option
import global/gtypes
import lntl_server/lntl_workers/toolkit/worker_types as wt
import users/types/users

pub type ClientState {
  // I am thinking of implementing a notification system,
  // that will notify you when you have multiple attempts,
  // on your account, and from which location it is coming
  // from. But obviously that needs its own timeline :)
  ClientState(
    user_id: users.UserId,
    user_auth_status: Bool,
    user_auth_attempt_count: Int,
    user_auth_method: option.Option(String),
    user_uptime: option.Option(Int),
    user_message_per_second: Int,
    user_ip_address: option.Option(String),
    // This should use the IpAddress format from Mist
    user_last_seen: option.Option(gtypes.LentilTimeStamp),
    user_geo_location: option.Option(String),
    user_message_count: Int,
    user_error_count: Int,
    user_resource_consumption_count: Int,
    user_session_subject: option.Option(
      process.Subject(wt.SessionOperationMessage),
    ),
  )
}

pub type ServerState {
  ServerState(
    server_node_id: option.Option(String),
    server_uptime: Int,
    // Ideally this should be a record containing information
    // like amount consumed, amount left, and the likes
    server_cpu_usage: Float,
    server_memory_usage: Float,
    server_active_rooms: Int,
    server_active_users: Int,
    server_message_throughput: Int,
    server_error_rate: Float,
    // Per Second
    server_last_heartbeat: option.Option(gtypes.LentilTimeStamp),
    server_disk_io: Float,
    server_queue_depth: Int,
    server_crashed_processes: Int,
    server_load_average: Float,
  )
}

pub type CentralStateAction {
  UPDATEUserAuthStatus(userid: users.UserId, status: Bool)
  UPDATEUserAuthAttempt(userid: users.UserId)
  UPDATEUserAuthMethod(userid: users.UserId, auth_method: option.Option(String))
  UPDATEUserUptime(userid: users.UserId, uptime: Int)
  UPDATEUserMessagePerSecond(userid: users.UserId, mps: Int)
  UPDATEUserIpAddress(userid: users.UserId, ip_addr: option.Option(String))
  UPDATEUserLastSeen(
    userid: users.UserId,
    last_seen: option.Option(gtypes.LentilTimeStamp),
  )
  UPDATEUserGeoLoation(userid: users.UserId, geo_loc: option.Option(String))
  UPDATEUserMessageCount(userid: users.UserId, count: Int)
  UPDATEUserErrorCount(userid: users.UserId, count: Int)
  UPDATEUserResourceConsumption(userid: users.UserId, amount: Int)
  UPDATEUserSessionSubject(
    userid: users.UserId,
    subject: option.Option(process.Subject(wt.SessionOperationMessage)),
  )
}

pub type ServerStateAction {
  UPDATEServerNodeId(node_id: option.Option(String))
  UPDATEServerUptime(uptime: Int)
  UPDATEServerCpuUsage(usage: Float)
  UPDATEServerMemoryUsage(usage: Float)
  UPDATEServerActiveRooms(rooms: Int)
  UPDATEServerActiveUsers(users: Int)
  UPDATEServerMessageThroughput(value: Int)
  UPDATEServerErrorRate(error_rate: Float)
  UPDATEServerQueueDepth(depth: Int)
  UPDATEServerCrashedProcesses(crashes: Int)
  UPDATEServerLoadAverage(load: Float)
}

pub type CentralState {
  CentralState(
    central_user_registry: dict.Dict(users.UserId, ClientState),
    central_server_state_subject: process.Subject(ServerStateAction),
  )
}
