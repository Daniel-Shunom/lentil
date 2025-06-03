import gleam/erlang/process

pub type ErrorCode {
  InvalidCall
  InvalidArgs
  InvalidTypes
  EmptyQueue
}

pub type Msg(val) {
  Push(val)
  Pop(reply: process.Subject(Result(val, ErrorCode)))
  PopMany(reply: process.Subject(Result(List(val), ErrorCode)), count: Int)
  Reset
  Shutdown
}
