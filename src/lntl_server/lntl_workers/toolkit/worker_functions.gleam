import gleam/erlang/process.{type Pid}
import gleam/list
import gleam/option.{None, Some}
import gleam/otp/actor.{type Next, Stop, continue}
import gleam/set
import lntl_server/lntl_workers/toolkit/constants as m
import lntl_server/lntl_workers/toolkit/worker_types.{
  type RoomSession, type RoomSessionMessage, type SessionOperationMessage,
  type UserSession, type UserSessionMessage, ANNOUNCE, CONNECT, DELETEROOM,
  DISCONNECT, FAILURE, JOIN, LEAVE, MEMBERTIMEOUT, PING, REMOVEMEMBER,
  RoomSession, SENDMESSAGE, SHUTDOWN, SUCCESS, UPDATECAPACITY, UPDATENAME,
}
import messages/types/msg.{
  type Message, type MessageStatus, DELETED, DELIVERED, DEQUED, EDITED,
  FAILEDPERM, FAILEDTEMP, Message, QUEUED, READ, SENDING, SENT, TIMEOUT, UNSENT,
}
import rooms/methods/methods.{set_capacity}
import rooms/types/rooms.{type Room, Room}
import users/types/users.{type User}

// This returns a long running process that 
// maintains the room state. It can only be 
// killed by the room owner.

pub fn room_session_handler(
  session_state: RoomSession,
  session_message: RoomSessionMessage,
) -> Next(SessionOperationMessage, RoomSession) {
  case session_message {
    DELETEROOM(user, client) -> {
      let userid = user.user_id
      let room_owner_id = session_state.room_data.room_owner.user_id
      case userid == room_owner_id {
        True -> {
          SUCCESS(m.room_delete_success)
          |> actor.send(client, _)
          Stop(process.Normal)
        }
        False -> {
          FAILURE(m.room_delete_failure)
          |> actor.send(client, _)
          continue(session_state)
        }
      }
    }
    SENDMESSAGE(message, client) -> {
      case validate_message(message) {
        True -> {
          // do something with the message, and change the status
          SUCCESS(m.room_msg_success)
          |> actor.send(client, _)
          continue(session_state)
        }
        False -> {
          let new_message = Message(..message, message_code: FAILEDTEMP)
          let new_bin = list.prepend(session_state.retry_bin, new_message)
          let new_state = RoomSession(..session_state, retry_bin: new_bin)
          FAILURE(m.room_msg_failure)
          |> actor.send(client, _)
          continue(new_state)
        }
      }
    }
    CONNECT(user, pid, client) -> {
      let search = fn(x) { x != user }
      case list.any(session_state.room_data.room_members, search) {
        True -> {
          let new_registry = set.insert(session_state.connection_registry, pid)
          let new_state =
            RoomSession(..session_state, connection_registry: new_registry)
          SUCCESS(m.room_connect_success)
          |> actor.send(client, _)
          continue(new_state)
        }
        False -> {
          FAILURE(m.room_connect_failure)
          |> actor.send(client, _)
          continue(session_state)
        }
      }
    }
    DISCONNECT(user, pid, client) -> {
      let search = fn(x) { x != user }
      case list.any(session_state.room_data.room_members, search) {
        True -> {
          let new_registry = set.delete(session_state.connection_registry, pid)
          let new_state =
            RoomSession(..session_state, connection_registry: new_registry)
          SUCCESS(m.client_room_disconnect_success)
          |> actor.send(client, _)
          continue(new_state)
        }
        False -> {
          FAILURE(m.client_room_connect_failure)
          |> actor.send(client, _)
          continue(session_state)
        }
      }
    }
    LEAVE(user, pid, client) -> {
      let new_members =
        session_state.room_data.room_members
        |> list.filter(fn(member) { member != user })
      let new_room = Room(..session_state.room_data, room_members: new_members)
      let new_registry = set.delete(session_state.connection_registry, pid)
      let new_state =
        RoomSession(
          ..session_state,
          room_data: new_room,
          connection_registry: new_registry,
        )
      SUCCESS(m.room_leave_success)
      |> actor.send(client, _)
      continue(new_state)
    }
    JOIN(user, pid, client) -> {
      let new_members =
        session_state.room_data.room_members
        |> list.prepend(user)
      let new_room = Room(..session_state.room_data, room_members: new_members)
      let new_registry = set.insert(session_state.connection_registry, pid)
      let new_state =
        RoomSession(
          ..session_state,
          room_data: new_room,
          connection_registry: new_registry,
        )
      SUCCESS(m.room_join_success)
      |> actor.send(client, _)
      continue(new_state)
    }
    UPDATENAME(user, new_name, client) -> {
      case session_state.room_data.room_owner == user {
        False -> {
          FAILURE(m.room_update_name_failure)
          |> actor.send(client, _)
          continue(session_state)
        }
        True -> {
          let new_room = Room(..session_state.room_data, room_name: new_name)
          let new_state = RoomSession(..session_state, room_data: new_room)
          SUCCESS(m.room_update_name_success)
          |> actor.send(client, _)
          continue(new_state)
        }
      }
    }
    UPDATECAPACITY(user, capacity, client) -> {
      case session_state.room_data.room_owner == user {
        False -> {
          FAILURE(m.room_update_capacity_failure)
          |> actor.send(client, _)
          continue(session_state)
        }
        True -> {
          let new_capacity = set_capacity(capacity)
          let new_room =
            Room(..session_state.room_data, room_capacity: new_capacity)
          let new_state = RoomSession(..session_state, room_data: new_room)
          SUCCESS(m.room_update_capacity_success)
          |> actor.send(client, _)
          continue(new_state)
        }
      }
    }
    ANNOUNCE(user, announcement, client) -> {
      todo
    }
    REMOVEMEMBER(user, client) -> {
      case session_state.room_data.room_owner == user {
        False -> {
          FAILURE(m.room_update_name_failure)
          |> actor.send(client, _)
          continue(session_state)
        }
        True -> {
          let new_members =
            session_state.room_data.room_members
            |> list.filter(fn(mem) { mem != user })
          let new_room =
            Room(..session_state.room_data, room_members: new_members)
          let new_state = RoomSession(..session_state, room_data: new_room)
          SUCCESS(m.room_remove_member_success)
          |> actor.send(client, _)
          continue(new_state)
        }
      }
    }
    MEMBERTIMEOUT(user, duration, client) -> {
      todo
    }
    PING(pid) -> {
      todo
    }
    SHUTDOWN -> {
      let msg = "Abnormal Activity"
      Stop(process.Abnormal(msg))
    }
  }
}

fn validate_message(msg: Message) -> Bool {
  todo as "will validate the message so it is not spam/obscene"
}
