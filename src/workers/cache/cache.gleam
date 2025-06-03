import gleam/erlang/process.{type Subject}
import gleam/list
import gleam/otp/actor
import workers/types

fn pop_many(state: List(a), count: Int) -> Result(List(a), types.ErrorCode) {
  case list.length(state) < count {
    True -> Error(types.InvalidArgs)
    False -> Ok(list.take(state, count))
  }
}

fn queue_handler(
  message: types.Msg(a),
  state: List(a),
) -> actor.Next(types.Msg(a), List(a)) {
  case message {
    types.Reset -> actor.continue([])
    types.Shutdown -> actor.Stop(process.Normal)
    types.Push(val) -> actor.continue(list.append(state, [val]))
    types.Pop(client) ->
      case state {
        [] -> {
          process.send(client, Error(types.EmptyQueue))
          actor.continue([])
        }
        [first, ..rest] -> {
          process.send(client, Ok(first))
          actor.continue(rest)
        }
      }
    types.PopMany(client, count) ->
      case state {
        [] -> {
          process.send(client, Error(types.EmptyQueue))
          actor.continue([])
        }
        _ -> {
          let reply = pop_many(state, count)
          let new_state = list.drop(state, count)
          process.send(client, reply)
          actor.continue(new_state)
        }
      }
  }
}

pub fn log_error(error: types.ErrorCode) -> String {
  case error {
    types.InvalidCall -> "Invalid Call for argument"
    types.InvalidArgs -> "Invalid Arguments supplied"
    types.InvalidTypes -> "Invalid Types supplied"
    types.EmptyQueue -> "Empty Queue in current actor"
  }
}

pub fn init_queue() -> Subject(types.Msg(a)) {
  let assert Ok(actor_subject) = actor.start([], queue_handler)
  actor_subject
}

pub fn send_msg(mailbox: Subject(types.Msg(a)), msg: types.Msg(a)) {
  actor.send(mailbox, msg)
}
