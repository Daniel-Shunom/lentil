import global_types/gtypes
import users/types/users

pub type MessageStatus {
  DEQUED
  QUEUED
  SENDING
  SENT
  UNSENT
  DELIVERED
  READ
  TIMEOUT
  FAILEDTEMP
  FAILEDPERM
  DELETED
  EDITED
}

pub type Message {
  Message(
    message_id: String,
    message_content: String,
    message_user_id: users.UserId,
    message_timestamp: gtypes.TimeStamp,
    message_code: MessageStatus,
  )
}

pub type MessageQueue {
  MessageQueue(
    user_id: users.UserId,
    msg_queue: List(Message),
    msg_bin: List(Message),
  )
}
