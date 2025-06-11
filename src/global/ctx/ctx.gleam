import gleam/dict
import gleam/erlang/process.{type Subject}
import gleam/otp/actor
import gleam/otp/task
import global/functions.{connect_lentildb}
import lntl_server/lntl_workers/toolkit/worker_functions as wf
import lntl_server/lntl_workers/toolkit/worker_types as wt
import pog
import rooms/types/rooms
import users/types/users.{type User, User}

pub type Context {
  Context(
    // this needs to be an actor mailbox
    connection_registry: dict.Dict(rooms.RoomId, String),
    db_connection: pog.Connection,
    supbox: Subject(SupMsg),
  )
}

pub fn get_context() -> Context {
  Context(
    connection_registry: dict.new(),
    db_connection: connect_lentildb(),
    supbox: sup_ctx(),
  )
}

type CtxState {
  CtxState(registry: dict.Dict(String, Subject(wt.SessionOperationMessage)))
}

pub type CtxMsg {
  AddToCtx(userid: String, usersubj: Subject(wt.SessionOperationMessage))
  DelFrmCtx(userid: String)
}

fn ctx() -> Subject(CtxMsg) {
  let assert Ok(ctx_subj) =
    CtxState(dict.new())
    |> actor.start(ctx_handler)
  ctx_subj
}

fn ctx_handler(msg: CtxMsg, state: CtxState) -> actor.Next(CtxMsg, CtxState) {
  case msg {
    AddToCtx(userid, usersubj) -> {
      let new_state = CtxState(dict.insert(state.registry, userid, usersubj))
      // could start the supervisor for that user here though?
      actor.continue(new_state)
    }
    DelFrmCtx(userid) -> {
      case dict.get(state.registry, userid) {
        Error(_) -> actor.continue(state)
        Ok(subj) -> {
          let task = task.async(fn() { actor.send(subj, wt.CLOSESESSION) })
          case task.try_await(task, 1000) {
            Error(_) -> actor.continue(state)
            Ok(_) -> {
              let new_state = CtxState(dict.delete(state.registry, userid))
              actor.continue(new_state)
            }
          }
        }
      }
    }
  }
}

pub type SupMsg {
  ADD(User)
  REM(String)
}

pub type SupState {
  SupState(supbox: Subject(SupMsg), ctx: Subject(CtxMsg))
}

fn sup_handler(msg: SupMsg, state: SupState) -> actor.Next(SupMsg, SupState) {
  case msg {
    ADD(user) -> {
      case wf.create_user_process(user) {
        Error(_) -> actor.continue(state)
        Ok(subj) -> {
          AddToCtx(user.user_id.id, subj.0)
          |> actor.send(state.ctx, _)
          actor.continue(state)
        }
      }
    }
    REM(userid) -> {
      DelFrmCtx(userid)
      |> actor.send(state.ctx, _)
      actor.continue(state)
    }
  }
}

fn sup_ctx() -> Subject(SupMsg) {
  let assert Ok(sup_subj) =
    SupState(supbox: process.new_subject(), ctx: ctx())
    |> actor.start(sup_handler)
  sup_subj
}
