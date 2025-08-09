
import gleam/int
import prng/seed
import prng/random
import server/workers/shared/shared_types as sm
import gleam/otp/actor
import gleam/erlang/process
import models/messages/types/msg
import gleam/dict
import global/ctx/types as t
import gleam/option.{type Option}
import models/users/types/users
import pog
import models/messages/methods/methods as mt
import gleam/otp/supervisor
import gleam/list
import gleam/function
import server/sql
import models/rooms/types/rooms

pub fn create_user_process(
  user: users.User,
  roomsupbox: process.Subject(t.RmMsg),
  ctx_subj: process.Subject(t.CtxMsg),
  conn: pog.Connection,
) -> Result(
  #(
    process.Subject(sm.UserSessionMessage),
    process.Subject(sm.RoomMessageStream),
  ),
  users.USERCREATIONERROR,
) {
  create_user_process_helper_supervisor(user, roomsupbox, ctx_subj, conn)
}

fn user_session_handler(
  session_message: sm.UserSessionMessage,
  session_state: sm.UserSession,
  roombox: process.Subject(t.RmMsg),
) -> actor.Next(sm.UserSessionMessage, sm.UserSession) {
  case session_message {
    sm.ShutdownUserSession -> actor.Stop(process.Normal)
    sm.Success(_) -> actor.continue(session_state)
    sm.MessageDelivered(_) -> {
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
          let new_state = sm.UserSession(..session_state, queue: nq)
          actor.continue(new_state)
        }
      }
    }
    sm.SendToRoom(roomid, message) -> {
      echo "----user process recieved message----"
      echo roomid
      echo "-----------------end-----------------"
      sm.ReceiveMessageFromUser(message, session_state.task_inbox)
      |> t.SEND(roomid.id, _)
      |> actor.send(roombox, _)
      actor.continue(session_state)
    }
    sm.AddToRoom(id, mailbox) -> {
      let new_rooms =
        session_state.member_rooms
        |> dict.insert(id, mailbox)
      let new_state = sm.UserSession(..session_state, member_rooms: new_rooms)
      actor.continue(new_state)
    }
    sm.Failure(message) -> {
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

fn message_stream_handler(
  message: sm.RoomMessageStream,
  state: Option(process.Subject(sm.RoomMessageStream)),
) -> actor.Next(
  sm.RoomMessageStream,
  Option(process.Subject(sm.RoomMessageStream)),
) {
  // TODO -> on the off chance that there are still emssages 
  // still in the user suject, we wait/add dely to clear those 
  // messages.
  case message {
    sm.Incoming(roomid, msg) -> {
      case state {
        option.None -> actor.continue(state)
        option.Some(subscriber) -> {
          sm.Incoming(roomid, msg)
          |> actor.send(subscriber, _)
          actor.continue(state)
        }
      }
    }
    sm.SubscribeWS(mailbox) -> {
      let new_state = option.Some(mailbox)
      actor.continue(new_state)
    }
    sm.UnsubscribeWS -> actor.continue(option.None)
  }
}

fn init_message_stream_mailbox(
  ws_inbox: Option(process.Subject(sm.RoomMessageStream)),
) -> process.Subject(sm.RoomMessageStream) {
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
  let new_session_id = generate_session_id()
  let ws_state = option.None
  let owned = get_owned_rooms(user, conn)
  let inbox = init_message_stream_mailbox(ws_state)
  let member =
    get_member_rooms(user, conn)
    |> list.map(fn(id) { #(id, process.new_subject()) })
    |> dict.from_list()
  let new_queue = mt.create_message_queue()
  let new_user_session =
    sm.UserSession(
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

fn new(
  arg: sm.UserSession,
  parent: process.Subject(process.Subject(sm.UserSessionMessage)),
  ws_inbox: Option(process.Subject(sm.RoomMessageStream)),
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

fn generate_session_id() -> String {
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
  
  "lntl-user-" <> secure_prefix <> secure_id
}