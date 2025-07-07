import gleam/dict
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/otp/actor
import gleam/otp/supervisor

// import gleam/otp/task
import pog

// import gleam/otp/static_supervisor.{OneForOne}
import gleam/set
import global/ctx/types as t
import lntl_server/lntl_workers/toolkit/constants as m
import lntl_server/lntl_workers/toolkit/worker_types as wt
import lntl_server/sql
import messages/methods/methods as mt
import messages/types/msg
import prng/random
import prng/seed

// import lntl_server/lntl_workers/w_room/w_session.
import gleam/function
import rooms/methods/methods
import rooms/types/rooms
import users/types/users

pub fn create_room_process(
  owner: users.UserId,
  cap: rooms.RoomCapacity,
  name: String,
  rm_registry: process.Subject(t.RmMsg),
  conn: pog.Connection,
) -> Result(
  #(process.Subject(wt.RoomSessionMessage), String),
  rooms.RoomCreateError,
) {
  create_room_process_helper_supervisor(owner, cap, rm_registry, name, conn)
}

pub fn create_user_process(
  user: users.User,
  roomsupbox: process.Subject(t.RmMsg),
  ctx_subj: process.Subject(t.CtxMsg),
  conn: pog.Connection,
) -> Result(
  #(
    process.Subject(wt.SessionOperationMessage),
    process.Subject(wt.RoomMessageStream),
  ),
  users.USERCREATIONERROR,
) {
  create_user_process_helper_supervisor(user, roomsupbox, ctx_subj, conn)
}

fn room_session_handler(
  session_message: wt.RoomSessionMessage,
  session_state: wt.RoomSession,
) -> actor.Next(wt.RoomSessionMessage, wt.RoomSession) {
  case session_message {
    wt.DELETEROOM(user, client) -> {
      let userid = user
      let room_owner_id = session_state.room_data.room_owner
      case userid == room_owner_id {
        True -> {
          wt.SUCCESS(m.room_delete_success)
          |> actor.send(client, _)
          actor.Stop(process.Normal)
        }
        False -> {
          wt.FAILURE(m.room_delete_failure)
          |> actor.send(client, _)
          actor.continue(session_state)
        }
      }
    }
    wt.SENDMESSAGE(message, _) -> {
      let new_message =
        msg.Message(..sanitize_message(message), message_code: msg.DELIVERED)
      // TODO -> PLEASE FIX THIS!!
      echo "::::IN ACTUAL ROOM::::"
      echo new_message
      let new_bin = list.prepend(session_state.retry_bin, new_message)
      let new_state = wt.RoomSession(..session_state, retry_bin: new_bin)
      // wt.INCOMING(roomid: session_state.room_data.room_id.id, message: message)
      // |> actor.send(client_taskinbox, _)
      let tmp_msg = wt.INCOMING(session_state.room_data.room_id.id, new_message)
      let mailman = fn() {
        echo "----------broadcasting----------"
        echo "SIZE OF POOL: "
          <> int.to_string(set.size(session_state.broadcast_pool))
        actor.send(_, tmp_msg)
      }
      set.each(session_state.broadcast_pool, mailman())
      actor.continue(new_state)
    }
    wt.CONNECT(userid, pid, client, client_mailbox) -> {
      let search = fn(x) { x != userid }
      case list.any(session_state.room_data.room_members, search) {
        True -> {
          let new_registry = set.insert(session_state.connection_registry, pid)
          let new_broadcast_pool =
            set.insert(session_state.broadcast_pool, client_mailbox)
          let new_state =
            wt.RoomSession(
              ..session_state,
              connection_registry: new_registry,
              broadcast_pool: new_broadcast_pool,
            )
          wt.SUCCESS(m.room_connect_success)
          |> actor.send(client, _)
          actor.continue(new_state)
        }
        False -> {
          wt.FAILURE(m.room_connect_failure)
          |> actor.send(client, _)
          actor.continue(session_state)
        }
      }
    }
    wt.DISCONNECT(userid, pid, client, client_mailbox) -> {
      let search = fn(x) { x != userid }
      case list.any(session_state.room_data.room_members, search) {
        True -> {
          let new_registry = set.delete(session_state.connection_registry, pid)
          let new_broadcast_pool =
            set.delete(session_state.broadcast_pool, client_mailbox)
          let new_state =
            wt.RoomSession(
              ..session_state,
              connection_registry: new_registry,
              broadcast_pool: new_broadcast_pool,
            )
          wt.SUCCESS(m.client_room_disconnect_success)
          |> actor.send(client, _)
          actor.continue(new_state)
        }
        False -> {
          wt.FAILURE(m.client_room_disconnect_failure)
          |> actor.send(client, _)
          actor.continue(session_state)
        }
      }
    }
    wt.LEAVE(user, pid, client) -> {
      let new_members =
        session_state.room_data.room_members
        |> list.filter(fn(member) { member != user })
      let new_room =
        rooms.Room(..session_state.room_data, room_members: new_members)
      let new_registry = set.delete(session_state.connection_registry, pid)
      let new_state =
        wt.RoomSession(
          ..session_state,
          room_data: new_room,
          connection_registry: new_registry,
        )
      wt.SUCCESS(m.room_leave_success)
      |> actor.send(client, _)
      actor.continue(new_state)
    }
    wt.JOIN(userid, pid, client) -> {
      case list.contains(session_state.room_data.room_members, userid) {
        True -> {
          let new_registry =
            session_state.connection_registry
            |> set.insert(pid)
          let new_state =
            wt.RoomSession(..session_state, connection_registry: new_registry)
          wt.SUCCESS("Already room member")
          |> actor.send(client, _)
          actor.continue(new_state)
        }
        False -> {
          let new_members =
            session_state.room_data.room_members
            |> list.prepend(userid)
          let new_room =
            rooms.Room(..session_state.room_data, room_members: new_members)
          let new_registry = set.insert(session_state.connection_registry, pid)
          let new_state =
            wt.RoomSession(
              ..session_state,
              room_data: new_room,
              connection_registry: new_registry,
            )
          wt.SUCCESS(m.room_join_success)
          |> actor.send(client, _)
          actor.continue(new_state)
        }
      }
    }
    wt.UPDATENAME(user, new_name, client) -> {
      case session_state.room_data.room_owner == user {
        False -> {
          wt.FAILURE(m.room_update_name_failure)
          |> actor.send(client, _)
          actor.continue(session_state)
        }
        True -> {
          let new_room =
            rooms.Room(..session_state.room_data, room_name: new_name)
          let new_state = wt.RoomSession(..session_state, room_data: new_room)
          wt.SUCCESS(m.room_update_name_success)
          |> actor.send(client, _)
          actor.continue(new_state)
        }
      }
    }
    wt.UPDATECAPACITY(user, capacity, client) -> {
      case session_state.room_data.room_owner == user {
        False -> {
          wt.FAILURE(m.room_update_capacity_failure)
          |> actor.send(client, _)
          actor.continue(session_state)
        }
        True -> {
          let new_capacity = methods.set_capacity(capacity)
          let new_room =
            rooms.Room(..session_state.room_data, room_capacity: new_capacity)
          let new_state = wt.RoomSession(..session_state, room_data: new_room)
          wt.SUCCESS(m.room_update_capacity_success)
          |> actor.send(client, _)
          actor.continue(new_state)
        }
      }
    }
    wt.ANNOUNCE(user, announcement, client) -> {
      case session_state.room_data.room_owner == user {
        False -> {
          wt.FAILURE(m.room_announce_failure)
          |> actor.send(client, _)
          actor.continue(session_state)
        }
        True -> {
          let new_anouncements =
            session_state.room_data.room_announcements
            |> list.prepend(announcement)
          let new_room =
            rooms.Room(
              ..session_state.room_data,
              room_announcements: new_anouncements,
            )
          let new_state = wt.RoomSession(..session_state, room_data: new_room)
          wt.SUCCESS(m.room_announce_success)
          |> actor.send(client, _)
          actor.continue(new_state)
        }
      }
    }
    wt.REMOVEMEMBER(user, client) -> {
      case session_state.room_data.room_owner == user {
        False -> {
          wt.FAILURE(m.room_update_name_failure)
          |> actor.send(client, _)
          actor.continue(session_state)
        }
        True -> {
          let new_members =
            session_state.room_data.room_members
            |> list.filter(fn(mem) { mem != user })
          let new_room =
            rooms.Room(..session_state.room_data, room_members: new_members)
          let new_state = wt.RoomSession(..session_state, room_data: new_room)
          wt.SUCCESS(m.room_remove_member_success)
          |> actor.send(client, _)
          actor.continue(new_state)
        }
      }
    }
    wt.PING(pid, client) -> {
      case set.contains(session_state.connection_registry, pid) {
        True -> {
          wt.SUCCESS(m.room_ping_success)
          |> actor.send(client, _)
          actor.continue(session_state)
        }
        False -> {
          let fail = "Cannot ping room"
          wt.FAILURE(fail)
          |> actor.send(client, _)
          actor.continue(session_state)
        }
      }
    }
    wt.SHUTDOWN -> {
      let msg = "Abnormal Activity"
      actor.Stop(process.Abnormal(msg))
    }
  }
}

fn user_session_handler(
  session_message: wt.SessionOperationMessage,
  session_state: wt.UserSession,
  roombox: process.Subject(t.RmMsg),
) -> actor.Next(wt.SessionOperationMessage, wt.UserSession) {
  case session_message {
    wt.CLOSESESSION -> actor.Stop(process.Normal)
    wt.SUCCESS(_) -> actor.continue(session_state)
    wt.MESSAGEDELIVERED(_) -> {
      echo "_____THE ROOM RESPOSNSE TO USER_____"
      echo session_message
      // TODO -> Okay we need to start thinking of things like caching, 
      // session states, and batching messages to databases for efficient
      // storage and retrieval of messages. Right now we just do nothing 
      // with it and obviously this is not best practice. :)
      case session_state.queue.msg_queue {
        [] -> actor.continue(session_state)
        [_, ..rest] -> {
          let nq = msg.MessageQueue(..session_state.queue, msg_queue: rest)
          let new_state = wt.UserSession(..session_state, queue: nq)
          actor.continue(new_state)
        }
      }
    }
    wt.SENDTOROOM(roomid, message) -> {
      echo "----user process recieved message----"
      echo roomid
      echo "-----------------end-----------------"
      wt.SENDMESSAGE(message, session_state.task_inbox)
      |> t.SEND(roomid.id, _)
      |> actor.send(roombox, _)
      actor.continue(session_state)
    }
    wt.ADDROOM(id, mailbox) -> {
      let new_rooms =
        session_state.member_rooms
        |> dict.insert(id, mailbox)
      let new_state = wt.UserSession(..session_state, member_rooms: new_rooms)
      actor.continue(new_state)
    }
    wt.FAILURE(message) -> {
      case message {
        // TODO -> please flesh out all error cases. 
        "" -> {
          actor.continue(session_state)
        }
        _ -> {
          actor.continue(session_state)
        }
      }
    }
  }
}

@external(erlang, "sanitizer", "sanitize_text")
fn clean(msg: String) -> String

fn sanitize_message(msg: msg.Message) -> msg.Message {
  let nc = clean(msg.message_content)
  msg.Message(..msg, message_content: nc)
}

fn create_room_process_helper_supervisor(
  room_owner owner: users.UserId,
  capacity cap: rooms.RoomCapacity,
  room_registry rm_registry: process.Subject(t.RmMsg),
  room_name name: String,
  connection conn: pog.Connection,
) -> Result(
  #(process.Subject(wt.RoomSessionMessage), String),
  rooms.RoomCreateError,
) {
  case methods.create_room(owner, name, [], cap, conn) {
    Error(error) -> Error(error)
    Ok(new_room) -> {
      let new_session_id = generate_session_id(wt.ROOMSESSION)
      let new_room_session =
        wt.RoomSession(
          room_data: new_room,
          session_id: new_session_id,
          retry_bin: [],
          connection_registry: set.new(),
          broadcast_pool: set.new(),
          // bin_handler: bin_strategy
        )
      let parent = process.new_subject()
      let worker =
        fn(_) {
          new_room_sup(
            new_room_session,
            new_room.room_id.id,
            parent,
            rm_registry,
          )
        }
        |> supervisor.worker()
      let assert Ok(_) =
        supervisor.start_spec(
          supervisor.Spec(Nil, 30, 5, supervisor.add(_, worker)),
        )

      let assert Ok(room_actor_subj) = process.receive(parent, 1000)
      Ok(#(room_actor_subj, new_session_id))
    }
  }
}

fn new_room_sup(
  session: wt.RoomSession,
  roomid: String,
  parent: process.Subject(process.Subject(wt.RoomSessionMessage)),
  rm_registry: process.Subject(t.RmMsg),
) {
  actor.start_spec(actor.Spec(
    fn() {
      let worker_subj = process.new_subject()
      process.send(parent, worker_subj)

      t.NEW(roomid, worker_subj)
      |> actor.send(rm_registry, _)

      process.new_selector()
      |> process.selecting(worker_subj, function.identity)
      |> actor.Ready(session, _)
    },
    1000,
    room_session_handler,
  ))
}

fn message_stream_handler(
  message,
  state: Option(process.Subject(wt.RoomMessageStream)),
) -> actor.Next(
  wt.RoomMessageStream,
  Option(process.Subject(wt.RoomMessageStream)),
) {
  // TODO -> on the off chance that there are still emssages 
  // still in the user suject, we wait/add dely to clear those 
  // messages.
  case message {
    wt.INCOMING(roomid, msg) -> {
      case state {
        option.None -> actor.continue(state)
        option.Some(subscriber) -> {
          wt.INCOMING(roomid, msg)
          |> actor.send(subscriber, _)
          actor.continue(state)
        }
      }
    }
    wt.SUBSCRIBEWS(mailbox) -> {
      let new_state = option.Some(mailbox)
      actor.continue(new_state)
    }
    wt.UNSUBSCRIBEWS -> actor.continue(option.None)
  }
}

fn init_message_stream_mailbox(
  ws_inbox: Option(process.Subject(wt.RoomMessageStream)),
) -> process.Subject(wt.RoomMessageStream) {
  let assert Ok(mailbox) = actor.start(ws_inbox, message_stream_handler)
  echo "started mailbox actor"
  echo mailbox
  mailbox
}

fn create_user_process_helper_supervisor(
  user: users.User,
  roomsupbox: process.Subject(t.RmMsg),
  ctx_subj: process.Subject(t.CtxMsg),
  conn: pog.Connection,
) {
  let new_session_id = generate_session_id(wt.USERSESSION)
  let ws_state = option.None
  let owned = get_owned_rooms(user, conn)
  let inbox = init_message_stream_mailbox(ws_state)
  let member =
    get_member_rooms(user, conn)
    |> list.map(fn(id) { #(id, process.new_subject()) })
    |> dict.from_list()
  let new_queue = mt.create_message_queue()
  let new_user_session =
    wt.UserSession(
      session_id: new_session_id,
      user: user,
      queue: new_queue,
      member_rooms: member,
      task_inbox: inbox,
      owned_rooms: owned,
    )
  let parent = process.new_subject()
  let worker =
    fn(_) { new(new_user_session, parent, ws_state, roomsupbox, ctx_subj) }
    |> supervisor.worker()

  let assert Ok(_) =
    supervisor.start_spec(
      supervisor.Spec(Nil, 25, 5, supervisor.add(_, worker)),
    )
  echo "::::::STARTED SUPERVISOR FOR USER::::::"
  let assert Ok(actor_subj) = process.receive(parent, 1000)
  echo "::::::RECIEVED ACTUAL PROCESS ACTOR FOR USER::::::"
  Ok(#(actor_subj, inbox))
}

// pub fn start_sup_supervisor(
//   owner: users.UserId,
//   cap: rooms.RoomCapacity,
//   rm_registry: process.Subject(t.RmMsg),
//   name: String,

// ) {
//   let assert Ok(#(subj, _)) = 
//     create_room_process_helper_supervisor(owner, cap, rm_registry, name)
//   let tmp = static_supervisor.new(OneForOne)
//   let worker = 
//     fn(_) { Ok(subj) }
//     |> supervisor.worker()
//     |> static_supervisor.add(tmp, _)

//   let spec =
//     static_supervisor.new(OneForOne)
//     |> static_supervisor.add(worker)
//     |> static_supervisor.restart_tolerance(5, 10)

//   let assert Ok(_) = static_supervisor.start_link(spec)

// }

fn new(
  arg: wt.UserSession,
  parent: process.Subject(process.Subject(wt.SessionOperationMessage)),
  ws_inbox: Option(process.Subject(wt.RoomMessageStream)),
  roomsupbox: process.Subject(t.RmMsg),
  ctx_subj: process.Subject(t.CtxMsg),
) {
  let handler = fn(message, session) {
    user_session_handler(message, session, roomsupbox)
  }
  echo "::::::CREATED SUPERVISOR FOR USER::::::"
  actor.start_spec(actor.Spec(
    fn() {
      echo "user supervisor's current user session state: "
      echo arg
      let worker_subj = process.new_subject()
      process.send(parent, worker_subj)
      t.AddToCtx(arg.user.user_id.id, #(
        worker_subj,
        init_message_stream_mailbox(ws_inbox),
      ))
      |> actor.send(ctx_subj, _)
      process.new_selector()
      |> process.selecting(worker_subj, function.identity)
      |> actor.Ready(arg, _)
    },
    1000,
    handler,
  ))
}

fn get_owned_rooms(user: users.User, conn: pog.Connection) -> List(rooms.RoomId) {
  case sql.fetch_room_by_id(conn, user.user_id.id) {
    Error(_) -> []
    Ok(res) -> {
      list.map(res.rows, fn(val) { rooms.RoomId(id: val.id) })
    }
  }
}

fn get_member_rooms(
  user: users.User,
  conn: pog.Connection,
) -> List(rooms.RoomId) {
  case sql.fetch_user_room_memberships(conn, user.user_id.id) {
    Error(_) -> []
    Ok(res) -> {
      list.map(res.rows, fn(val) { rooms.RoomId(id: val.room_id) })
    }
  }
}

fn generate_session_id(id_type: wt.SESSIONTYPE) -> String {
  let str = random.fixed_size_string(32)
  let num = random.int(0, 100_000)
  let secure_prefix =
    random.int(0, 13)
    |> random.random_sample()
    |> seed.new()
    |> random.sample(num, _)
    |> int.to_string()

  let secure_id =
    random.int(0, 9)
    |> random.random_sample()
    |> seed.new()
    |> random.sample(str, _)

  case id_type {
    // TODO -> improve this later pls
    wt.ROOMSESSION -> "lntl-rm-" <> secure_prefix <> secure_id
    wt.USERSESSION -> "lntl-user-" <> secure_prefix <> secure_id
  }
}
