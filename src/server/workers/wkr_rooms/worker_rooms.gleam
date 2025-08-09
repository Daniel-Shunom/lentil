import prng/seed
import prng/random
import server/workers/shared/shared_types as sm
import models/rooms/methods/methods
import models/rooms/types/rooms
import server/workers/toolkit/constants as m
import gleam/int
import models/messages/types/msg
import gleam/otp/actor
import gleam/list
import gleam/set
import gleam/erlang/process
import gleam/function
import global/ctx/types as t
import models/users/types/users
import pog
import gleam/otp/supervisor

pub fn create_room_process(
  owner: users.UserId,
  cap: rooms.RoomCapacity,
  name: String,
  rm_registry: process.Subject(t.RmMsg),
  conn: pog.Connection,
) -> Result(
  #(process.Subject(sm.RoomSessionMessage), String),
  rooms.RoomCreateError,
) {
  create_room_process_helper_supervisor(owner, cap, rm_registry, name, conn)
}

fn new_room_sup(
  session: sm.RoomSession,
  roomid: String,
  parent: process.Subject(process.Subject(sm.RoomSessionMessage)),
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

fn create_room_process_helper_supervisor(
  room_owner owner: users.UserId,
  capacity cap: rooms.RoomCapacity,
  room_registry rm_registry: process.Subject(t.RmMsg),
  room_name name: String,
  connection conn: pog.Connection,
) -> Result(
  #(process.Subject(sm.RoomSessionMessage), String),
  rooms.RoomCreateError,
) {
  case methods.create_room(owner, name, [], cap, conn) {
    Error(error) -> Error(error)
    Ok(new_room) -> {
      let new_session_id = generate_session_id()
      let new_room_session =
        sm.RoomSession(
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

fn room_session_handler(
  session_message: sm.RoomSessionMessage,
  session_state:   sm.RoomSession
) -> actor.Next(sm.RoomSessionMessage, sm.RoomSession) {
  case session_message {
    sm.DeleteRoom(user, client) -> {
      let userid = user
      let room_owner_id = session_state.room_data.room_owner
      case userid == room_owner_id {
        True -> {
          sm.Success(m.room_delete_success)
          |> actor.send(client, _)
          actor.Stop(process.Normal)
        }
        False -> {
          sm.Failure(m.room_delete_failure)
          |> actor.send(client, _)
          actor.continue(session_state)
        }
      }
    }

    sm.SendMessageFromRoom(_user, _subj) ->  actor.continue(session_state)
    sm.ReceiveMessageFromUser(message, _) -> {
      let new_message =
        msg.Message(..sanitize_message(message), message_code: msg.DELIVERED)
      // TODO -> PLEASE FIX THIS!!
      echo "::::IN ACTUAL ROOM::::"
      echo new_message
      let new_bin = list.prepend(session_state.retry_bin, new_message)
      let new_state = sm.RoomSession(..session_state, retry_bin: new_bin)
      // wt.INCOMING(roomid: session_state.room_data.room_id.id, message: message)
      // |> actor.send(client_taskinbox, _)
      let tmp_msg = sm.Incoming(session_state.room_data.room_id.id, new_message)
      let mailman = fn() {
        echo "----------broadcasting----------"
        echo "SIZE OF POOL: "
          <> int.to_string(set.size(session_state.broadcast_pool))
        actor.send(_, tmp_msg)
      }
      set.each(session_state.broadcast_pool, mailman())
      actor.continue(new_state)
    }
    sm.ConnectUser(userid, pid, client, client_mailbox) -> {
      let search = fn(x) { x != userid }
      case list.any(session_state.room_data.room_members, search) {
        True -> {
          let new_registry = set.insert(session_state.connection_registry, pid)
          let new_broadcast_pool =
            set.insert(session_state.broadcast_pool, client_mailbox)
          let new_state =
            sm.RoomSession(
              ..session_state,
              connection_registry: new_registry,
              broadcast_pool: new_broadcast_pool,
            )
          sm.Success(m.room_connect_success)
          |> actor.send(client, _)
          actor.continue(new_state)
        }
        False -> {
          sm.Failure(m.room_connect_failure)
          |> actor.send(client, _)
          actor.continue(session_state)
        }
      }
    }
    sm.DisconnectUser(userid, pid, client, client_mailbox) -> {
      let search = fn(x) { x != userid }
      case list.any(session_state.room_data.room_members, search) {
        True -> {
          let new_registry = set.delete(session_state.connection_registry, pid)
          let new_broadcast_pool =
            set.delete(session_state.broadcast_pool, client_mailbox)
          let new_state =
            sm.RoomSession(
              ..session_state,
              connection_registry: new_registry,
              broadcast_pool: new_broadcast_pool,
            )
          sm.Success(m.client_room_disconnect_success)
          |> actor.send(client, _)
          actor.continue(new_state)
        }
        False -> {
          sm.Failure(m.client_room_disconnect_failure)
          |> actor.send(client, _)
          actor.continue(session_state)
        }
      }
    }
    sm.LeaveRoom(user, pid, client) -> {
      let new_members =
        session_state.room_data.room_members
        |> list.filter(fn(member) { member != user })
      let new_room =
        rooms.Room(..session_state.room_data, room_members: new_members)
      let new_registry = set.delete(session_state.connection_registry, pid)
      let new_state =
        sm.RoomSession(
          ..session_state,
          room_data: new_room,
          connection_registry: new_registry,
        )
      sm.Success(m.room_leave_success)
      |> actor.send(client, _)
      actor.continue(new_state)
    }
    sm.JoinRoom(userid, pid, client) -> {
      case list.contains(session_state.room_data.room_members, userid) {
        True -> {
          let new_registry =
            session_state.connection_registry
            |> set.insert(pid)
          let new_state =
            sm.RoomSession(..session_state, connection_registry: new_registry)
          sm.Success("Already room member")
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
            sm.RoomSession(
              ..session_state,
              room_data: new_room,
              connection_registry: new_registry,
            )
          sm.Success(m.room_join_success)
          |> actor.send(client, _)
          actor.continue(new_state)
        }
      }
    }
    sm.UpdateRoomName(user, new_name, client) -> {
      case session_state.room_data.room_owner == user {
        False -> {
          sm.Failure(m.room_update_name_failure)
          |> actor.send(client, _)
          actor.continue(session_state)
        }
        True -> {
          let new_room =
            rooms.Room(..session_state.room_data, room_name: new_name)
          let new_state = sm.RoomSession(..session_state, room_data: new_room)
          sm.Success(m.room_update_name_success)
          |> actor.send(client, _)
          actor.continue(new_state)
        }
      }
    }
    sm.UpdateRoomCapacity(user, capacity, client) -> {
      case session_state.room_data.room_owner == user {
        False -> {
          sm.Failure(m.room_update_capacity_failure)
          |> actor.send(client, _)
          actor.continue(session_state)
        }
        True -> {
          let new_capacity = methods.set_capacity(capacity)
          let new_room =
            rooms.Room(..session_state.room_data, room_capacity: new_capacity)
          let new_state = sm.RoomSession(..session_state, room_data: new_room)
          sm.Success(m.room_update_capacity_success)
          |> actor.send(client, _)
          actor.continue(new_state)
        }
      }
    }
    sm.AnnounceToRoom(user, announcement, client) -> {
      case session_state.room_data.room_owner == user {
        False -> {
          sm.Failure(m.room_announce_failure)
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
          let new_state = sm.RoomSession(..session_state, room_data: new_room)
          sm.Success(m.room_announce_success)
          |> actor.send(client, _)
          actor.continue(new_state)
        }
      }
    }
    sm.RemoveFromRoom(user, client) -> {
      case session_state.room_data.room_owner == user {
        False -> {
          sm.Failure(m.room_update_name_failure)
          |> actor.send(client, _)
          actor.continue(session_state)
        }
        True -> {
          let new_members =
            session_state.room_data.room_members
            |> list.filter(fn(mem) { mem != user })
          let new_room =
            rooms.Room(..session_state.room_data, room_members: new_members)
          let new_state = sm.RoomSession(..session_state, room_data: new_room)
          sm.Success(m.room_remove_member_success)
          |> actor.send(client, _)
          actor.continue(new_state)
        }
      }
    }
    sm.Ping(pid, client) -> {
      case set.contains(session_state.connection_registry, pid) {
        True -> {
          sm.Success(m.room_ping_success)
          |> actor.send(client, _)
          actor.continue(session_state)
        }
        False -> {
          let fail = "Cannot ping room"
          sm.Failure(fail)
          |> actor.send(client, _)
          actor.continue(session_state)
        }
      }
    }
    sm.ShutdownRoom-> {
      let msg = "Abnormal Activity"
      actor.Stop(process.Abnormal(msg))
    }
  }
}


@external(erlang, "sanitizer", "sanitize_text")
fn clean(msg: String) -> String


fn sanitize_message(msg: msg.Message) -> msg.Message {
  let nc = clean(msg.message_content)
  msg.Message(..msg, message_content: nc)
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
  
  "lntl-rm-" <> secure_prefix <> secure_id
}