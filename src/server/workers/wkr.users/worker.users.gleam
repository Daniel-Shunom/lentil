import gleam/dict.{type Dict}
import gleam/erlang/process
import gleam/otp/actor
import server/workers/toolkit/worker_types as wt
import models/users/types/users
import wisp

pub type UserSessionRegistryState {
  UserSessionRegistryState(
    children: Dict(String, process.Pid),
    by_user: Dict(String, String),
  )
}

pub type UserSessionRegistryMessage {
  STOP(
    session_id: String,
    reply_to: process.Subject(wt.SessionOperationMessage),
  )
  EXIT(String)
  KILL(String)
  START(
    user: users.User,
    user_subj: process.Subject(wt.SessionOperationMessage),
    session_id: String,
    reply_to: process.Subject(wt.SessionOperationMessage),
  )
}

pub fn start_user_supervisor() -> process.Subject(UserSessionRegistryMessage) {
  let init = UserSessionRegistryState(children: dict.new(), by_user: dict.new())
  let assert Ok(subj) = actor.start(init, user_supervisor_handler)
  subj
}

fn user_supervisor_handler(
  msg: UserSessionRegistryMessage,
  state: UserSessionRegistryState,
) -> actor.Next(UserSessionRegistryMessage, UserSessionRegistryState) {
  case msg {
    START(user, user_subj, session_id, reply_to) -> {
      process.trap_exits(True)
      process.subject_owner(user_subj)
      |> process.link()
      let children2 =
        state.children
        |> dict.insert(session_id, process.subject_owner(user_subj))
      let recorded_user =
        state.by_user
        |> dict.insert(user.user_id.id, session_id)
      reply_to
      |> actor.send(wt.SUCCESS(session_id))

      actor.continue(UserSessionRegistryState(
        children: children2,
        by_user: recorded_user,
      ))
    }
    STOP(session_id, reply_to) -> {
      case dict.get(state.children, session_id) {
        Error(_) -> {
          wt.FAILURE("No such session")
          |> actor.send(reply_to, _)
          actor.continue(state)
        }
        Ok(child_pid) -> {
          process.send_exit(child_pid)
          wt.CLOSESESSION
          |> actor.send(reply_to, _)
          actor.continue(state)
        }
      }
    }
    EXIT(session_id) -> {
      let children2 = dict.delete(state.children, session_id)
      wisp.log_error("UserSession #{session_id} crashed")
      actor.continue(UserSessionRegistryState(..state, children: children2))
    }
    KILL(session_id) -> {
      let children2 = dict.delete(state.children, session_id)
      wisp.log_error("UserSession #{session_id} crashed")
      actor.continue(UserSessionRegistryState(..state, children: children2))
    }
  }
}
