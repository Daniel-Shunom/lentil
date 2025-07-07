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
    userid: users.UserId,
    auth_status: Bool,
    auth_attempt_count: Int,
    auth_method: option.Option(String),
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

pub type CentralClientState {
  CentralClientState(registry: dict.Dict(users.UserId, ClientState))
}
