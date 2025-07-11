import gleam/erlang/process
import gleam/function.{identity}
import gleam/list
import gleam/option
import gleam/otp/actor
import gleam/otp/supervisor
import lntl_frontline/central/manager
import lntl_frontline/central/state
import lntl_frontline/msg_types as mt
import messages/types/msg
import users/types/users

pub fn init_global_router_actor() -> process.Subject(
  mt.GlobalMonitorMessage(msg.Message),
) {
  let state =
    mt.GlobalMonitorState(
      server: process.new_subject(),
      client: process.new_subject(),
      central_state: manager.start_central_state(),
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
  central_router_state: mt.GlobalMonitorState(msg.Message),
) -> actor.Next(
  mt.GlobalMonitorMessage(msg.Message),
  mt.GlobalMonitorState(msg.Message),
) {
  case central_router_message {
    mt.ClientRouterMessage(client_message) -> {
      // To send messages, I am using a list of messages to be fired at once
      // given that we can save number of times we would have to rewrite the 
      // actor.send logic
      case client_message {
        mt.CLIENTAuthEvent(userid, _roomid, is_authenticated, _time) -> {
          let messages = [
            state.UPDATEUserAuthAttempt(users.UserId(userid), 1),
            state.UPDATEUserAuthStatus(users.UserId(userid), is_authenticated),
          ]
          {
            use message <- list.each(messages)
            actor.send(central_router_state.central_state, message)
          }
          actor.continue(central_router_state)
        }
        mt.CLIENTMessageEvent(userid, _roomid, auth, msg_count) -> {
          case auth {
            False -> {
              state.DELETEUserState(users.UserId(userid))
              |> actor.send(central_router_state.central_state, _)
              actor.continue(central_router_state)
            }
            True -> {
              let messages = [
                state.UPDATEUserMessageCount(users.UserId(userid), msg_count),
              ]
              {
                use message <- list.each(messages)
                actor.send(central_router_state.central_state, message)
              }
              actor.continue(central_router_state)
            }
          }
        }
        mt.CLIENTRoomEvent(userid, roomid, auth, event_type, time) -> {
          todo
        }
        mt.CLIENTFetchResource(userid, auth, resource_type, time) -> {
          let messages = [
            // We should pass in a count instead.
            state.UPDATEUserResourceConsumption(users.UserId(userid), 1),
          ]
          {
            use message <- list.each(messages)
            actor.send(central_router_state.central_state, message)
          }
          actor.continue(central_router_state)
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
