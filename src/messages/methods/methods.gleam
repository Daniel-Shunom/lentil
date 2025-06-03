import gleam/list
import gleam/option.{type Option, None, Some}
import global_types/gtypes
import messages/types/msg
import users/types/users

pub fn create_message(
  message_id msid: String,
  message_content mscontent: String,
  message_sender_id ms_sender_id: users.UserId,
  message_timestamp stmp: gtypes.TimeStamp,
  message_code code: msg.MessageStatus,
) -> msg.Message {
  msg.Message(
    message_id: msid,
    message_content: mscontent,
    message_user_id: ms_sender_id,
    message_timestamp: stmp,
    message_code: code,
  )
}

pub fn create_message_queue(sender_id id: users.UserId) -> msg.MessageQueue {
  msg.MessageQueue(user_id: id, msg_queue: [], msg_bin: [], buffer_size: 1024)
}

pub fn add_message_to_queue(
  new_message nmsg: msg.Message,
  message_queue queue: msg.MessageQueue,
) -> msg.MessageQueue {
  let new_message = msg.Message(..nmsg, message_code: msg.QUEUED)
  case list.length(queue.msg_queue) < queue.buffer_size {
    False -> queue
    True -> {
      let new_queue = list.append(queue.msg_queue, [new_message])
      msg.MessageQueue(..queue, msg_queue: new_queue)
    }
  }
}

pub fn add_message_to_bin(
  new_message nmsg: msg.Message,
  message_bin bin: msg.MessageQueue,
) -> msg.MessageQueue {
  let new_bin = list.prepend(bin.msg_bin, nmsg)
  case list.length(bin.msg_bin) < bin.buffer_size {
    True -> msg.MessageQueue(..bin, msg_bin: new_bin)
    False -> bin
  }
}

pub fn pop_message_from_queue(
  message_queue queue: msg.MessageQueue,
) -> Option(#(msg.Message, msg.MessageQueue)) {
  case queue.msg_queue {
    [] -> None
    [message, ..rest] -> {
      let new_msg_queue = msg.MessageQueue(..queue, msg_queue: rest)
      let popped_message = msg.Message(..message, message_code: msg.DEQUED)
      Some(#(popped_message, new_msg_queue))
    }
  }
}

pub fn pop_message_from_bin(
  message_bin bin: msg.MessageQueue,
) -> Option(#(msg.Message, msg.MessageQueue)) {
  case bin.msg_bin {
    [] -> None
    [message, ..rest] -> {
      let new_msg_bin = msg.MessageQueue(..bin, msg_bin: rest)
      let popped_message = msg.Message(..message, message_code: msg.DEQUED)
      Some(#(popped_message, new_msg_bin))
    }
  }
}

pub fn clear_message_queue(
  msg_queue queue: msg.MessageQueue,
) -> msg.MessageQueue {
  msg.MessageQueue(..queue, msg_queue: [])
}

pub fn clear_message_bin(msg_queue queue: msg.MessageQueue) -> msg.MessageQueue {
  msg.MessageQueue(..queue, msg_bin: [])
}

pub fn get_message_status(message msg: msg.Message) -> msg.MessageStatus {
  msg.message_code
}

pub fn handle_message_queue(
  msg_queue queue: msg.MessageQueue,
) -> msg.MessageQueue {
  case queue.msg_queue {
    [] -> queue
    [head, ..rest] ->
      case get_message_status(head) {
        msg.DEQUED | msg.DELETED | msg.DELIVERED ->
          msg.MessageQueue(..queue, msg_queue: rest)
        msg.TIMEOUT | msg.FAILEDTEMP -> {
          let updated_bin = list.prepend(queue.msg_bin, head)
          msg.MessageQueue(..queue, msg_bin: updated_bin)
        }
        _ -> queue
      }
  }
}

// TODO -> visit later and think more about this
pub fn handle_message_bin(
  msg_bin bin: msg.MessageQueue,
  handler fnc: fn(msg.Message) -> msg.Message,
) -> msg.MessageQueue {
  case bin.msg_bin {
    [] -> bin
    [head, ..rest] ->
      case get_message_status(fnc(head)) {
        msg.TIMEOUT | msg.FAILEDPERM -> msg.MessageQueue(..bin, msg_bin: rest)
        msg.QUEUED ->
          case list.length(bin.msg_queue) < bin.buffer_size {
            False -> bin
            True -> {
              let new_queue = list.append(bin.msg_queue, [head])
              msg.MessageQueue(..bin, msg_queue: new_queue)
            }
          }
        _ -> bin
      }
  }
}
