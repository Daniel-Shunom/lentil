import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/otp/actor
import gleam/set
import lntl_server/lntl_workers/toolkit/constants as m
import lntl_server/lntl_workers/toolkit/worker_types as wt
import messages/types/msg
import prng/random
import prng/seed
import rooms/methods/methods
import rooms/types/rooms
import users/types/users

pub fn create_room_process(
  owner: users.User,
  cap: rooms.RoomCapacity,
  name: String,
) -> Result(process.Subject(wt.RoomSessionMessage), rooms.RoomCreateError) {
  create_room_process_helper(owner, cap, name)
}

fn room_session_handler(
  session_message: wt.RoomSessionMessage,
  session_state: wt.RoomSession,
) -> actor.Next(wt.RoomSessionMessage, wt.RoomSession) {
  case session_message {
    wt.DELETEROOM(user, client) -> {
      let userid = user.user_id
      let room_owner_id = session_state.room_data.room_owner.user_id
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
    wt.SENDMESSAGE(message, client) -> {
      let new_message = sanitize_message(message)
      // TODO -> PLEASE FIX THIS!!
      let new_bin = list.prepend(session_state.retry_bin, new_message)
      let new_state = wt.RoomSession(..session_state, retry_bin: new_bin)
      wt.FAILURE(m.room_msg_failure)
      |> actor.send(client, _)
      actor.continue(new_state)
    }
    wt.CONNECT(user, pid, client) -> {
      let search = fn(x) { x != user }
      case list.any(session_state.room_data.room_members, search) {
        True -> {
          let new_registry = set.insert(session_state.connection_registry, pid)
          let new_state =
            wt.RoomSession(..session_state, connection_registry: new_registry)
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
    wt.DISCONNECT(user, pid, client) -> {
      let search = fn(x) { x != user }
      case list.any(session_state.room_data.room_members, search) {
        True -> {
          let new_registry = set.delete(session_state.connection_registry, pid)
          let new_state =
            wt.RoomSession(..session_state, connection_registry: new_registry)
          wt.SUCCESS(m.client_room_disconnect_success)
          |> actor.send(client, _)
          actor.continue(new_state)
        }
        False -> {
          wt.FAILURE(m.client_room_connect_failure)
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
    wt.JOIN(user, pid, client) -> {
      let new_members =
        session_state.room_data.room_members
        |> list.prepend(user)
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

@external(erlang, "sanitizer", "sanitize_text")
fn clean(msg: String) -> String

fn sanitize_message(msg: msg.Message) -> msg.Message {
  let nc = clean(msg.message_content)
  msg.Message(..msg, message_content: nc)
}

fn create_room_process_helper(
  room_owner owner: users.User,
  capacity cap: rooms.RoomCapacity,
  room_name name: String,
) -> Result(process.Subject(wt.RoomSessionMessage), rooms.RoomCreateError) {
  case methods.create_room(owner, name, [], cap) {
    Error(error) -> Error(error)
    Ok(new_room) -> {
      let new_session_id = generate_session_id(wt.ROOMSESSION)
      let new_registry: set.Set(process.Pid) = set.new()
      let new_room_session =
        wt.RoomSession(
          room_data: new_room,
          session_id: new_session_id,
          retry_bin: [],
          connection_registry: new_registry,
        )
      let assert Ok(new_process) =
        actor.start(new_room_session, room_session_handler)
      Ok(new_process)
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
