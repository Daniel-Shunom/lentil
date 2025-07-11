import gleam/otp/supervisor
import gleam/function
import gleam/dict
import gleam/erlang/process
import gleam/option
import gleam/otp/actor
import lntl_frontline/central/state
import users/types/users

pub fn start_central_state() -> state.CentralState {
  state.CentralState(
    central_user_registry: dict.new(),
    central_server_state_subject: start_server_state(),
  )
}

// When a user is authenticated, we start the state and initialize the auth as true
pub fn new_client_state(userid: users.UserId) -> state.ClientState {
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
    user_session_subject: option.None,
  )
}


/// This server process and supervisor is to be managed internally
/// and not to be used by external consumers.
fn start_server_state() {
  let server_state = new_server_state()
  let parent = process.new_subject()
  let worker = 
    fn(_) { new_server_state_supervisor(server_state, parent) }
    |> supervisor.worker()
  
  let assert Ok(_) =
    supervisor.start_spec(supervisor.Spec(Nil, 25, 5, supervisor.add(_, worker)))

  let assert Ok(server_state_subj) =
    process.receive(parent, 1000)
  server_state_subj
}

fn new_server_state_supervisor(
  state: state.ServerState,
  parent: process.Subject(process.Subject(state.ServerStateAction))
) {
  actor.start_spec(actor.Spec(
    fn() {
      let worker_subj = process.new_subject()
      process.send(parent, worker_subj)
      process.new_selector()
      |> process.selecting(worker_subj, function.identity)
      |> actor.Ready(state, _)
    },
    1000,
    server_state_handler
  ))
}

fn new_server_state() -> state.ServerState {
  state.ServerState(
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
    server_load_average: 0.0,
  )
}

fn server_state_handler(
  server_message: state.ServerStateAction,
  server_state: state.ServerState,
) -> actor.Next(state.ServerStateAction, state.ServerState) {
  case server_message {
    state.UPDATEServerNodeId(node_id) -> {
      let new_state = state.ServerState(..server_state, server_node_id: node_id)
      actor.continue(new_state)
    }
    state.UPDATEServerUptime(update) -> {
      let new_state =
        state.ServerState(
          ..server_state,
          server_uptime: server_state.server_uptime + update,
        )
      actor.continue(new_state)
    }
    state.UPDATEServerCpuUsage(usage) -> {
      let new_state =
        state.ServerState(
          ..server_state,
          server_cpu_usage: server_state.server_cpu_usage +. usage,
        )
      actor.continue(new_state)
    }
    state.UPDATEServerMemoryUsage(usage) -> {
      let new_state =
        state.ServerState(
          ..server_state,
          server_memory_usage: server_state.server_memory_usage +. usage,
        )
      actor.continue(new_state)
    }
    state.UPDATEServerActiveRooms(rooms) -> {
      let new_state =
        state.ServerState(
          ..server_state,
          server_active_rooms: server_state.server_active_rooms + rooms,
        )
      actor.continue(new_state)
    }
    state.UPDATEServerActiveUsers(users) -> {
      let new_state =
        state.ServerState(
          ..server_state,
          server_active_users: server_state.server_active_users + users,
        )
      actor.continue(new_state)
    }
    state.UPDATEServerMessageThroughput(update) -> {
      let new_state =
        state.ServerState(
          ..server_state,
          server_message_throughput: server_state.server_message_throughput
            + update,
        )
      actor.continue(new_state)
    }
    state.UPDATEServerErrorRate(error_rate) -> {
      let new_state =
        state.ServerState(
          ..server_state,
          server_error_rate: server_state.server_error_rate +. error_rate,
        )
      actor.continue(new_state)
    }
    state.UPDATEServerQueueDepth(depth) -> {
      let new_state =
        state.ServerState(
          ..server_state,
          server_queue_depth: server_state.server_queue_depth + depth,
        )
      actor.continue(new_state)
    }
    state.UPDATEServerCrashedProcesses(crashes) -> {
      let new_state =
        state.ServerState(
          ..server_state,
          server_crashed_processes: server_state.server_crashed_processes
            + crashes,
        )
      actor.continue(new_state)
    }
    state.UPDATEServerLoadAverage(load) -> {
      let new_state =
        state.ServerState(
          ..server_state,
          server_load_average: server_state.server_load_average +. load,
        )
      actor.continue(new_state)
    }
  }
}
