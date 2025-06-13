import gleam/dict
import gleam/erlang/process.{type Subject}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/otp/task
import global/ctx/types as t
import global/functions.{connect_lentildb, get_timestamp, id_generator}
import lntl_server/lntl_workers/toolkit/worker_functions as wf
import lntl_server/lntl_workers/toolkit/worker_types as wt
import lntl_server/sql
import messages/methods/methods
import messages/types/msg
import pog
import rooms/types/rooms
import users/types/users

// Public Types / APIs

pub type Context {
  Context(
    connection_registry: dict.Dict(rooms.RoomId, String),
    db_connection: pog.Connection,
    usersupbox: Subject(t.SupMsg),
    roomsupbox: Subject(t.RmSupMsg),
  )
}

pub fn get_context() -> Context {
  let connection = connect_lentildb()
  let roombox = rmctxprx()
  let roomsup = room_supervisor(roombox)
  let usersup = sup_ctx(roombox)
  case get_rooms(connection) {
    None -> {
      Context(
        connection_registry: dict.new(),
        db_connection: connection,
        usersupbox: usersup,
        roomsupbox: roomsup,
      )
    }
    Some(listofinstructions) -> {
      let _ =
        task.async(fn() {
          listofinstructions
          |> list.each(fn(instruction) {
            instruction
            |> actor.send(roomsup, _)
          })
        })
      Context(
        connection_registry: dict.new(),
        db_connection: connection,
        usersupbox: usersup,
        roomsupbox: roomsup,
      )
    }
  }
}

pub fn room_sup_handler(
  msg: t.RmSupMsg,
  state: t.RmSupState,
) -> actor.Next(t.RmSupMsg, t.RmSupState) {
  case msg {
    t.DELROOM(sessionid) -> {
      t.DEL(sessionid)
      |> actor.send(state.context, _)
      actor.continue(state)
    }
    t.NEWROOM(userid, capacity, name) ->
      case wf.create_room_process(userid, capacity, name) {
        Error(_) -> actor.continue(state)
        Ok(#(roomproc, sessionid)) -> {
          t.NEW(sessionid, roomproc)
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

// Context Functions

fn ctx() -> Subject(t.CtxMsg) {
  let assert Ok(ctx_subj) =
    t.CtxState(dict.new())
    |> actor.start(ctx_handler)
  ctx_subj
}

fn ctx_handler(
  msg: t.CtxMsg,
  state: t.CtxState,
) -> actor.Next(t.CtxMsg, t.CtxState) {
  case msg {
    t.AddToCtx(userid, usersubj) ->
      case dict.has_key(state.registry, userid) {
        True -> {
          echo state.registry
          actor.continue(state)
        }
        False -> {
          let new_state =
            t.CtxState(dict.insert(state.registry, userid, usersubj))
          echo new_state.registry
          actor.continue(new_state)
        }
      }
    t.DelFrmCtx(userid) ->
      case dict.get(state.registry, userid) {
        Error(_) -> actor.continue(state)
        Ok(subj) -> {
          let task = task.async(fn() { actor.send(subj, wt.CLOSESESSION) })
          case task.try_await(task, 1000) {
            Error(_) -> actor.continue(state)
            Ok(_) -> {
              let new_state = t.CtxState(dict.delete(state.registry, userid))
              actor.continue(new_state)
            }
          }
        }
      }
    t.MsgToUserProc(userid, roomid, message) -> {
      case dict.get(state.registry, roomid) {
        Error(val) -> {
          echo val
          actor.continue(state)
        }
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
          echo new_msessage
          wt.SENDTOROOM(rooms.RoomId(roomid), new_msessage)
          |> actor.send(usersession, _)
          actor.continue(state)
        }
      }
    }
  }
}

// Supervisor Functions

fn sup_ctx(roomsupbox: Subject(t.RmMsg)) -> Subject(t.SupMsg) {
  let assert Ok(sup_subj) =
    t.SupState(process.new_subject(), ctx(), roomsupbox:)
    |> actor.start(sup_handler)
  sup_subj
}

fn sup_handler(
  msg: t.SupMsg,
  state: t.SupState,
) -> actor.Next(t.SupMsg, t.SupState) {
  case msg {
    t.ADD(user) ->
      case wf.create_user_process(user, state.roomsupbox) {
        Error(_) -> actor.continue(state)
        Ok(subj) -> {
          t.AddToCtx(user.user_id.id, subj.0)
          |> actor.send(state.ctx, _)
          actor.continue(state)
        }
      }
    t.REM(userid) -> {
      t.DelFrmCtx(userid)
      |> actor.send(state.ctx, _)
      actor.continue(state)
    }
    t.MSG(userid, roomid, message) -> {
      t.MsgToUserProc(userid.id, roomid, message)
      |> actor.send(state.ctx, _)
      actor.continue(state)
    }
  }
}

// Room Context Functions

fn rmctxprx() -> Subject(t.RmMsg) {
  let assert Ok(rmsubj) =
    t.RmState(dict.new())
    |> actor.start(rmhandler)
  rmsubj
}

fn rmhandler(msg: t.RmMsg, state: t.RmState) -> actor.Next(t.RmMsg, t.RmState) {
  case msg {
    t.NEW(roomid, roomsubj) ->
      case dict.has_key(state.registry, roomid) {
        True -> actor.continue(state)
        False -> {
          let new_state =
            t.RmState(dict.insert(state.registry, roomid, roomsubj))
          actor.continue(new_state)
        }
      }
    t.DEL(roomid) ->
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
              let new_state = t.RmState(new_registry)
              actor.continue(new_state)
            }
          }
        }
      }
    t.SEND(roomid, msg) -> {
      case dict.get(state.registry, roomid) {
        Error(_) -> actor.continue(state)
        Ok(roomsubj) -> {
          actor.send(roomsubj, msg)
          actor.continue(state)
        }
      }
    }
  }
}

// Room Supervisor Functions

fn room_supervisor(roombox: Subject(t.RmMsg)) -> Subject(t.RmSupMsg) {
  let assert Ok(room_sup_subj) =
    t.RmSupState(process.new_subject(), roombox)
    |> actor.start(room_sup_handler)
  room_sup_subj
}

fn get_rooms(conn: pog.Connection) -> Option(List(t.RmSupMsg)) {
  let newtask =
    task.async(fn() {
      case sql.fetch_all_rooms(conn) {
        Error(_) -> []
        Ok(pog.Returned(_, rows)) -> {
          case rows {
            [] -> []
            _ -> {
              list.map(rows, fn(x) {
                t.NEWROOM(users.UserId(x.id), cap(x.capacity), x.name)
              })
            }
          }
        }
      }
    })
  case task.await_forever(newtask) {
    [] -> None
    val -> Some(val)
  }
}

fn cap(num: Int) -> rooms.RoomCapacity {
  case num {
    128 -> rooms.SMALL
    256 -> rooms.MEDIUM
    512 -> rooms.LARGE
    _ -> rooms.SMALL
  }
}
