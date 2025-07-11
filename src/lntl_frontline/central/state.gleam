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
  // from. But obviously that needs it's own timeline :)
  ClientState(
    user_id: users.UserId,
    user_auth_status: Bool,
    user_auth_attempt_count: Int,
    user_auth_method: option.Option(String),
    user_uptime: option.Option(Int),
    user_message_per_second: Int,
    user_ip_address: option.Option(String),
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
    server_cpu_usage: Float,
    server_memory_usage: Float,
    server_active_rooms: Int,
    server_active_users: Int,
    server_message_throughput: Int,
    server_error_rate: Float,
    server_last_heartbeat: option.Option(gtypes.LentilTimeStamp),
    server_disk_io: Float,
    server_queue_depth: Int,
    server_crashed_processes: Int,
    server_load_average: Float,
  )
}

pub type CentralState {
  CentralState(
    central_user_registry: dict.Dict(users.UserId, ClientState),
    central_server_state: ServerState,
  )
}
