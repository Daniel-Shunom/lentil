import gleam/erlang/process
import gleam/function.{identity}
import gleam/otp/actor
import gleam/otp/supervisor
import lntl_frontline/msg_types as mt
import messages/types/msg

pub fn init_global_router_actor() -> process.Subject(
  mt.GlobalMonitorMessage(msg.Message),
) {
  let state =
    mt.GlobalMonitorState(
      server: process.new_subject(),
      client: process.new_subject(),
    )
  let parent = process.new_subject()
  let worker =
    fn(_) { init_actor(parent, state) }
    |> supervisor.worker()
  let assert Ok(_) =
    supervisor.start_spec(
      supervisor.add(_, worker)
      |> supervisor.Spec(Nil, 25, 5, _),
    )
  let assert Ok(global_router_subject) = process.receive(parent, 1000)
  global_router_subject
}

fn init_actor(
  parent: process.Subject(process.Subject(mt.GlobalMonitorMessage(msg.Message))),
  central_router_state: mt.GlobalMonitorState(msg.Message),
) {
  actor.start_spec(actor.Spec(
    fn() {
      let worker_subj = process.new_subject()
      process.send(parent, worker_subj)
      process.new_selector()
      |> process.selecting(worker_subj, identity)
      |> actor.Ready(central_router_state, _)
    },
    1000,
    global_router_handler,
  ))
}

pub fn global_router_handler(
  central_router_message: mt.GlobalMonitorMessage(msg.Message),
  _central_router_state: mt.GlobalMonitorState(msg.Message),
) -> actor.Next(
  mt.GlobalMonitorMessage(msg.Message),
  mt.GlobalMonitorState(msg.Message),
) {
  case central_router_message {
    mt.ClientRouterMessage(client_message) -> {
      case client_message {
        mt.CLIENTAuthEvent(userid, roomid, auth, time) -> {
          todo
        }
        mt.CLIENTMessageEvent(userid, roomid, auth, msg) -> {
          todo
        }
        mt.CLIENTRoomEvent(userid, roomid, auth, event_type, time) -> {
          todo
        }
        mt.CLIENTFetchResource(userid, auth, resource_type, time) -> {
          todo
        }
        mt.CLIENTTokenIssued(userid, method, time) -> {
          todo
        }
        mt.CLIENTTokenRevoked(userid, reason, time) -> {
          todo
        }
        mt.CLIENTProtocolViolation(userid, raw, time) -> {
          todo
        }
        mt.CLIENTMalformedMessage(userid, raw, time) -> {
          todo
        }
        mt.CLIENTLatencyReport(userid, ping_ms, time) -> {
          todo
        }
      }
    }
    mt.ServerRouterMessage(server_message) -> {
      case server_message {
        mt.SERVERRoomState(
          roomid,
          status,
          members,
          active_members,
          message_channel_statue,
        ) -> {
          todo
        }
        mt.SERVERUserThrottled(userid, reason) -> {
          todo
        }
        mt.SERVERUserBlocked(userid, reason) -> {
          todo
        }
        mt.SERVERNodeLoad(cpu, mem, time) -> {
          todo
        }
        mt.SERVERProcessCrash(process_id, reason) -> {
          todo
        }
      }
    }
  }
}
