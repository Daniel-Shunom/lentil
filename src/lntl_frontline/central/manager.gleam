import users/types/users
import lntl_frontline/central/state
import gleam/option
import gleam/dict

pub fn start_central_state() -> state.CentralState {
  state.CentralState(
    central_user_registry: dict.new(),
    central_server_state: state.ServerState(
      server_node_id: option.None,
      server_uptime: 0,
      server_cpu_usage: 0.0,
      server_memory_usage: 0.0,
      server_active_rooms: 0,
      server_active_users: 0,
      server_message_throughput: 0,
      server_error_rate: 0.0,
      server_last_heartbeat: option.None,
      server_disk_io: 0.0,
      server_queue_depth: 0,
      server_crashed_processes: 0,
      server_load_average: 0.0
    )
  )
}

// When a user is authenticated, we start the state and initialize the auth as true
pub fn start_client_state(userid: users.UserId) -> state.ClientState {
  state.ClientState(
    user_id: userid,
    user_auth_status: True,
    user_auth_attempt_count: 1,
    // gonna resolve this auth method to hold 
    // specific methods sometime soon
    user_auth_method: option.None,
    user_uptime: option.None,
    user_message_per_second: 0,
    user_ip_address: option.None,
    user_last_seen: option.None,
    user_geo_location: option.None,
    user_message_count: 0,
    user_error_count: 0,
    // Okay so this is actually pretty stupid
    // because it does not even indicate which
    // resource is being consumed. MUST FIX later
    user_resource_consumption_count: 0,
    user_session_subject: option.None
  )
}