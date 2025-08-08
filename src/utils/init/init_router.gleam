import gleam/erlang/process
import gleam/function.{identity}
import gleam/list
import gleam/option.{None, Some}
import gleam/otp/actor
import gleam/otp/supervisor
import gleam/string
import models/messages/types/msg
import models/users/types/users
import utils/central/manager
import utils/central/state
import utils/msg_types as mt

pub fn init_global_router_actor() -> process.Subject(
  mt.GlobalMonitorMessage(msg.Message),
) {
  let state =
    mt.GlobalMonitorState(
      server: process.new_subject(),
      client: process.new_subject(),
      central_state: manager.start_central_state(),
      stream_channel: None,
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

          let _ = {
            use subj <- option.then(central_router_state.stream_channel)
            let msg = "CLIENTSIDE EVENT: " <> string.inspect(client_message)
            actor.send(subj, msg)
            None
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

              let _ = {
                use subj <- option.then(central_router_state.stream_channel)
                let msg = "CLIENTSIDE EVENT: " <> string.inspect(client_message)
                actor.send(subj, msg)
                None
              }

              actor.continue(central_router_state)
            }
          }
        }
        mt.CLIENTRoomEvent(userid, roomid, auth, event_type, time) -> {
          let _ = {
            use subj <- option.then(central_router_state.stream_channel)
            let msg = "CLIENTSIDE EVENT: " <> string.inspect(client_message)
            actor.send(subj, msg)
            None
          }

          actor.continue(central_router_state)
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

          let _ = {
            use subj <- option.then(central_router_state.stream_channel)
            let msg = "CLIENTSIDE EVENT: " <> string.inspect(client_message)
            actor.send(subj, msg)
            None
          }

          actor.continue(central_router_state)
        }
        mt.CLIENTTokenIssued(userid, method, time) -> {
          let _ = {
            use subj <- option.then(central_router_state.stream_channel)
            let msg = "CLIENTSIDE EVENT: " <> string.inspect(client_message)
            actor.send(subj, msg)
            None
          }

          actor.continue(central_router_state)
        }
        mt.CLIENTTokenRevoked(userid, reason, time) -> {
          let _ = {
            use subj <- option.then(central_router_state.stream_channel)
            let msg = "CLIENTSIDE EVENT: " <> string.inspect(client_message)
            actor.send(subj, msg)
            None
          }

          state.DELETEUserState(users.UserId(userid))
          |> actor.send(central_router_state.central_state, _)
          actor.continue(central_router_state)
        }
        mt.CLIENTProtocolViolation(userid, raw, time) -> {
          let _ = {
            use subj <- option.then(central_router_state.stream_channel)
            let msg = "CLIENTSIDE EVENT: " <> string.inspect(client_message)
            actor.send(subj, msg)
            None
          }

          actor.continue(central_router_state)
        }
        mt.CLIENTMalformedMessage(userid, raw, time) -> {
          let _ = {
            use subj <- option.then(central_router_state.stream_channel)
            let msg = "CLIENTSIDE EVENT: " <> string.inspect(client_message)
            actor.send(subj, msg)
            None
          }

          state.DELETEUserState(users.UserId(userid))
          |> actor.send(central_router_state.central_state, _)
          actor.continue(central_router_state)
        }
        mt.CLIENTLatencyReport(userid, ping_ms, time) -> {
          let _ = {
            use subj <- option.then(central_router_state.stream_channel)
            let msg = "CLIENTSIDE EVENT: " <> string.inspect(client_message)
            actor.send(subj, msg)
            None
          }

          actor.continue(central_router_state)
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
          actor.continue(central_router_state)
        }
        mt.SERVERUserThrottled(userid, reason) -> {
          actor.continue(central_router_state)
        }
        mt.SERVERUserBlocked(userid, reason) -> {
          actor.continue(central_router_state)
        }
        mt.SERVERNodeLoad(cpu, mem, time) -> {
          let _ = {
            use subj <- option.then(central_router_state.stream_channel)
            let msg = "SERVERSIDE EVENT: " <> string.inspect(server_message)
            actor.send(subj, msg)
            None
          }
          actor.continue(central_router_state)
        }
        mt.SERVERProcessCrash(process_id, reason) -> {
          actor.continue(central_router_state)
        }
      }
    }
    mt.StreamChannelSubject(new_subject) -> {
      let new_state =
        mt.GlobalMonitorState(
          ..central_router_state,
          stream_channel: Some(new_subject),
        )
      actor.continue(new_state)
    }
  }
}
