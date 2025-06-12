import gleam/dict
import gleam/erlang/process.{type Subject}
import gleam/otp/actor
import gleam/otp/task
import global/functions.{connect_lentildb, get_timestamp, id_generator}
import lntl_server/lntl_workers/toolkit/worker_functions as wf
import lntl_server/lntl_workers/toolkit/worker_types as wt
import messages/methods/methods
import messages/types/msg
import pog
import rooms/types/rooms
import users/types/users.{type User, type UserId}

// Public Types / APIs

pub type Context {
  Context(
    connection_registry: dict.Dict(rooms.RoomId, String),
    db_connection: pog.Connection,
    usersupbox: Subject(SupMsg),
    roomsupbox: Subject(RmSupMsg),
  )
}

pub type SupMsg {
  ADD(User)
  REM(String)
  MSG(userid: UserId, roomid: String, message: String)
}

pub type CtxMsg {
  AddToCtx(userid: String, usersubj: Subject(wt.SessionOperationMessage))
  MsgToUserProc(userid: String, roomid: String, roommessage: String)
  DelFrmCtx(userid: String)
}

pub type RmMsg {
  NEW(sessionid: String, sessionmailbox: Subject(wt.RoomSessionMessage))
  DEL(sessionid: String)
}

pub type RmSupMsg {
  DELROOM(sessionid: String)
  NEWROOM(userid: UserId, capacity: rooms.RoomCapacity, roomname: String)
}

pub fn get_context() -> Context {
  Context(
    connection_registry: dict.new(),
    db_connection: connect_lentildb(),
    usersupbox: sup_ctx(),
    roomsupbox: room_supervisor(),
  )
}

pub fn room_sup_handler(
  msg: RmSupMsg,
  state: RmSupState,
) -> actor.Next(RmSupMsg, RmSupState) {
  case msg {
    DELROOM(sessionid) -> {
      DEL(sessionid)
      |> actor.send(state.context, _)
      actor.continue(state)
    }
    NEWROOM(userid, capacity, name) ->
      case wf.create_room_process(userid, capacity, name) {
        Error(_) -> actor.continue(state)
        Ok(#(roomproc, sessionid)) -> {
          NEW(sessionid, roomproc)
          |> actor.send(state.context, _)
          actor.continue(state)
        }
      }
  }
}

// Private Types

// TODO -> currently messages go too far downstream before validation occurs. I think that
// is unnecessary and we can shorten that step by moving the registry upstream by one actor
// layer

type CtxState {
  CtxState(registry: dict.Dict(String, Subject(wt.SessionOperationMessage)))
}

type SupState {
  SupState(supbox: Subject(SupMsg), ctx: Subject(CtxMsg))
}

type RmState {
  RmState(registry: dict.Dict(String, Subject(wt.RoomSessionMessage)))
}

pub type RmSupState {
  RmSupState(room_supervisorbox: Subject(RmSupMsg), context: Subject(RmMsg))
}

// Context Functions

fn ctx() -> Subject(CtxMsg) {
  let assert Ok(ctx_subj) =
    CtxState(dict.new())
    |> actor.start(ctx_handler)
  ctx_subj
}

fn ctx_handler(msg: CtxMsg, state: CtxState) -> actor.Next(CtxMsg, CtxState) {
  case msg {
    AddToCtx(userid, usersubj) ->
      case dict.has_key(state.registry, userid) {
        True -> {
          echo state.registry
          actor.continue(state)
        }
        False -> {
          let new_state =
            CtxState(dict.insert(state.registry, userid, usersubj))
          echo new_state.registry
          actor.continue(new_state)
        }
      }
    DelFrmCtx(userid) ->
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
    MsgToUserProc(userid, roomid, message) -> {
      case dict.get(state.registry, userid) {
        Error(_) -> actor.continue(state)
        Ok(usersession) -> {
          let msgid = id_generator("lntl-msg")
          let new_msessage =
            methods.create_message(
              msgid,
              message,
              users.UserId(userid),
              get_timestamp(),
              msg.QUEUED,
            )
          wt.SENDTOROOM(rooms.RoomId(roomid), new_msessage)
          |> actor.send(usersession, _)
          actor.continue(state)
        }
      }
    }
  }
}

// Supervisor Functions

fn sup_ctx() -> Subject(SupMsg) {
  let assert Ok(sup_subj) =
    SupState(process.new_subject(), ctx())
    |> actor.start(sup_handler)
  sup_subj
}

fn sup_handler(msg: SupMsg, state: SupState) -> actor.Next(SupMsg, SupState) {
  case msg {
    ADD(user) ->
      case wf.create_user_process(user) {
        Error(_) -> actor.continue(state)
        Ok(subj) -> {
          AddToCtx(user.user_id.id, subj.0)
          |> actor.send(state.ctx, _)
          actor.continue(state)
        }
      }
    REM(userid) -> {
      DelFrmCtx(userid)
      |> actor.send(state.ctx, _)
      actor.continue(state)
    }
    MSG(userid, roomid, message) -> {
      MsgToUserProc(userid.id, roomid, message)
      |> actor.send(state.ctx, _)
      actor.continue(state)
    }
  }
}

// Room Context Functions

fn rmctxprx() -> Subject(RmMsg) {
  let assert Ok(rmsubj) =
    RmState(dict.new())
    |> actor.start(rmhandler)
  rmsubj
}

fn rmhandler(msg: RmMsg, state: RmState) -> actor.Next(RmMsg, RmState) {
  case msg {
    NEW(roomid, roomsubj) ->
      case dict.has_key(state.registry, roomid) {
        True -> actor.continue(state)
        False -> {
          let new_state = RmState(dict.insert(state.registry, roomid, roomsubj))
          actor.continue(new_state)
        }
      }
    DEL(roomid) ->
      case dict.get(state.registry, roomid) {
        Error(_) -> actor.continue(state)
        Ok(roomsubj) -> {
          let task =
            fn() { actor.send(roomsubj, wt.SHUTDOWN) }
            |> task.async()
          case task.try_await(task, 10_000) {
            Error(_) -> actor.continue(state)
            Ok(_) -> {
              let new_registry = dict.delete(state.registry, roomid)
              let new_state = RmState(new_registry)
              actor.continue(new_state)
            }
          }
        }
      }
  }
}

// Room Supervisor Functions

fn room_supervisor() -> Subject(RmSupMsg) {
  let assert Ok(room_sup_subj) =
    RmSupState(process.new_subject(), rmctxprx())
    |> actor.start(room_sup_handler)
  room_sup_subj
}
