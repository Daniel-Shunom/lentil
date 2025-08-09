import gleam/dict
import gleam/erlang/process.{type Subject}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/otp/task
import global/ctx/types as t
import global/functions.{get_timestamp, id_generator}
import models/messages/methods/methods
import models/messages/types/msg
import models/rooms/types/rooms
import models/users/types/users
import pog
import server/sql
import server/workers/wkr_rooms/worker_rooms
import server/workers/wkr_users/worker_users
import server/workers/shared/shared_types as sm
import utils/msg_types as mt

// Public Types / APIs

pub type Context {
  Context(
    connection_registry: dict.Dict(rooms.RoomId, String),
    db_connection: pog.Connection,
    usersupbox: Subject(t.SupMsg),
    roomsupbox: Subject(t.RmSupMsg),
    server_monitor: Subject(mt.GlobalMonitorMessage(msg.Message)),
  )
}

pub fn get_context(
  conn: pog.Connection,
  roombox_subj: process.Subject(t.RmMsg),
  supstate_box: process.Subject(t.SupMsg),
  monitor: Subject(mt.GlobalMonitorMessage(msg.Message)),
) -> Context {
  let connection = conn
  let roombox = roombox_subj
  let roomsup = room_supervisor(roombox, conn, supstate_box)
  let usersup = supstate_box
  case get_rooms(connection) {
    None -> {
      echo "No rooms in DB"
      Context(
        connection_registry: dict.new(),
        db_connection: connection,
        usersupbox: usersup,
        roomsupbox: roomsup,
        server_monitor: monitor,
      )
    }
    Some(listofinstructions) -> {
      listofinstructions
      |> list.each(fn(instruction) {
        echo instruction
        instruction
        |> actor.send(roomsup, _)
      })
      Context(
        connection_registry: dict.new(),
        db_connection: connection,
        usersupbox: usersup,
        roomsupbox: roomsup,
        server_monitor: monitor,
      )
    }
  }
}

pub fn room_sup_handler(
  msg: t.RmSupMsg,
  state: t.RmSupState,
  conn: pog.Connection,
  rmsup: process.Subject(t.SupMsg),
) -> actor.Next(t.RmSupMsg, t.RmSupState) {
  let shipment = process.new_subject()
  case msg {
    t.DELROOM(sessionid) -> {
      t.DEL(sessionid)
      |> actor.send(state.context, _)
      actor.continue(state)
    }

    t.NEWROOM(userid, capacity, name, roomid) ->
      case worker_rooms.create_room_process(userid, capacity, name, state.context, conn) {
        Error(_) -> actor.continue(state)
        Ok(#(roomproc, _)) -> {
          echo "==========ROOMMSG=========="
          echo roomid
          echo roomproc
          t.NEW(roomid, roomproc)
          |> actor.send(state.context, _)
          actor.continue(state)
        }
      }

    t.ADDTOBROADCAST(userid, roomid, ws_inbox) -> {
      echo "******************************"
      echo "*                            *"
      echo "*  Adding user to broadcast  *"
      echo "*                            *"
      echo "******************************"
      echo "*                             "
      echo "*  USERID: " <> userid.id
      echo "*  ROOMID: " <> roomid
      echo "*                             "
      echo "******************************"
      t.GETUSERSESSION(userid, shipment)
      |> process.send(rmsup, _)

      case process.receive(shipment, 1000) {
        Error(_) -> {
          echo "failed to get user process from registry"
          actor.continue(state)
        }
        Ok(data) -> {
          echo "fetched user process from registry: "
          echo data
          t.BCT(roomid, userid.id, data.0, data.1)
          |> actor.send(state.context, _)
          sm.SubscribeWS(mailbox: ws_inbox)
          |> actor.send(data.1, _)
          actor.continue(state)
        }
      }
      actor.continue(state)
    }

    t.REMOVEFROMBROADCAST(userid, roomid) -> {
      let temp_s = process.new_subject()
      t.GETUSERSESSION(userid, temp_s)
      |> process.send(rmsup, _)

      case process.receive(temp_s, 1000) {
        Error(_) -> {
          echo "failed to get user process from registry"
          actor.continue(state)
        }
        Ok(data) -> {
          echo "fetched user process from registry: "
          echo data
          t.RMBCT(roomid, userid.id, data.0, data.1)
          |> actor.send(state.context, _)
          actor.continue(state)
        }
      }
    }
  }
}

pub fn init_supstate(rmctx: Subject(t.RmMsg)) {
  t.SupState(ctx(), rmctx)
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
    t.AddToCtx(userid, usersubjects) -> {
      echo "::::::::::::NEW USER PROCESS::::::::::::"
      echo "subjects: "
      echo usersubjects
      let new_state =
        t.CtxState(dict.insert(state.registry, userid, usersubjects))
      actor.continue(new_state)
    }
    t.DelFrmCtx(userid) ->
      case dict.get(state.registry, userid) {
        Error(_) -> actor.continue(state)
        Ok(subj) -> {
          let task = task.async(fn() { actor.send(subj.0, sm.ShutdownUserSession) })
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
      echo "CTX_HANDLER::::   " <> message

      case dict.get(state.registry, userid) {
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
          echo "VALIDATEDMESSAGE::::   " <> new_msessage.message_content
          sm.SendToRoom(rooms.RoomId(roomid), new_msessage)
          |> actor.send(usersession.0, _)
          actor.continue(state)
        }
      }
    }
    t.FetchUserSession(userid, subj) -> {
      case dict.get(state.registry, userid) {
        Error(_) -> {
          echo "failed to retrieve session details from registry"
          actor.continue(state)
        }
        Ok(res) -> {
          echo "sucessfully retrieved user session details from registry"
          process.send(subj, res)
          actor.continue(state)
        }
      }
    }
  }
}

// Supervisor Functions
pub fn sup_ctx(supstate: t.SupState, conn: pog.Connection) -> Subject(t.SupMsg) {
  let assert Ok(sup_subj) = {
    use msg, state <- actor.start(supstate)
    sup_handler(msg, state, conn)
  }
  sup_subj
}

fn sup_handler(
  msg: t.SupMsg,
  state: t.SupState,
  conn: pog.Connection,
) -> actor.Next(t.SupMsg, t.SupState) {
  case msg {
    t.ADD(user) ->
      case worker_users.create_user_process(user, state.roomsupbox, state.ctx, conn) {
        Error(_) -> actor.continue(state)
        Ok(subj) -> {
          t.AddToCtx(user.user_id.id, subj)
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
    t.GETUSERSESSION(userid, reply_to) -> {
      let ctx_shipment = process.new_subject()
      // subject for listeneing to code
      // case dict.get(state.ctx, userid) { todo }
      echo "recieved incoming GETUSERSESSION message"
      echo msg

      t.FetchUserSession(userid.id, ctx_shipment)
      |> actor.send(state.ctx, _)
      case process.receive(ctx_shipment, 5000) {
        Error(_) -> {
          echo "failed to Fetch user subjects"
          actor.continue(state)
        }
        Ok(response) -> {
          echo "successfully fetched user subjects"
          process.send(reply_to, response)
          actor.continue(state)
        }
      }
    }
  }
}

// Room Context Functions

pub fn rmctxprx() -> Subject(t.RmMsg) {
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
          echo "++++++adding to room registry++++++"
          echo new_state.registry
          actor.continue(new_state)
        }
      }
    t.DEL(roomid) ->
      case dict.get(state.registry, roomid) {
        Error(_) -> actor.continue(state)
        Ok(roomsubj) -> {
          let task =
            fn() { actor.send(roomsubj, sm.ShutdownRoom) }
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
    t.BCT(roomid, userid, user_client, user_mailbox) -> {
      echo "-------NEW CONNECTION REQUEST-------"
      case dict.get(state.registry, roomid) {
        Error(_) -> actor.continue(state)
        Ok(roomsubj) -> {
          sm.ConnectUser(
            userid: users.UserId(userid),
            process_id: process.subject_owner(user_mailbox),
            user_session_process: user_client,
            user_mailbox_process: user_mailbox,
          )
          |> actor.send(roomsubj, _)
          actor.continue(state)
        }
      }
    }
    t.RMBCT(roomid, userid, user_client, ws_inbox) -> {
      case dict.get(state.registry, roomid) {
        // TODO -> ideally, we should be able to bubble messages
        // back up to the client to notify of errors, instead of 
        // passing generic error messages.
        Error(_) -> actor.continue(state)
        Ok(roomsubj) -> {
          let userpid = process.subject_owner(user_client)
          sm.DisconnectUser(users.UserId(userid), userpid, user_client, ws_inbox)
          |> actor.send(roomsubj, _)
          actor.continue(state)
        }
      }
    }
    t.SEND(roomid, msg) -> {
      echo "::::IN ROOM SUP::::   "
      echo msg
      case dict.get(state.registry, roomid) {
        Error(_) -> actor.continue(state)
        Ok(roomsubj) -> {
          echo "::::ROOMSUBJ::::"
          echo roomsubj
          actor.send(roomsubj, msg)
          actor.continue(state)
        }
      }
    }
  }
}

// Room Supervisor Functions

fn room_supervisor(
  roombox: Subject(t.RmMsg),
  conn: pog.Connection,
  supstate_box: process.Subject(t.SupMsg),
) -> Subject(t.RmSupMsg) {
  let assert Ok(room_sup_subj) = {
    // t.RmSupState(process.new_subject(), roombox)
    // |> actor.start(room_sup_handler())
    use msg, state <- actor.start(t.RmSupState(process.new_subject(), roombox))
    room_sup_handler(msg, state, conn, supstate_box)
  }
  room_sup_subj
}

fn get_rooms(conn: pog.Connection) -> Option(List(t.RmSupMsg)) {
  let newtask = {
    use <- task.async()
    case sql.fetch_all_rooms(conn) {
      Error(_) -> []
      Ok(pog.Returned(_, rows)) -> {
        use x <- list.map(rows)
        echo x
        t.NEWROOM(users.UserId(x.id), cap(x.capacity), x.name, x.id)
      }
    }
  }
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
