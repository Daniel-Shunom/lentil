import gleam/erlang/process
import gleam/function
import gleam/list
import gleam/otp/actor
import gleam/otp/supervisor
import gleam/otp/task
import server/lntl_workers/toolkit/worker_types as wt
import models/messages/types/msg

// TODO -> wire this up into the room process creator function
pub fn create_bin_handler(state: wt.BinState) {
  let parent = process.new_subject()
  let worker =
    fn(_) { new(state, parent) }
    |> supervisor.worker()

  let assert Ok(_) =
    supervisor.start_spec(
      supervisor.Spec(Nil, 10, 5, supervisor.add(_, worker)),
    )

  let assert Ok(bin_subject) = process.receive(parent, 1000)
  bin_subject
}

fn bin_handler(
  msg: wt.Retry,
  state: wt.BinState,
) -> actor.Next(wt.Retry, wt.BinState) {
  let wt.Retry(value) = msg
  let tmp = msg.Message(..value, message_code: msg.QUEUED)
  let retry_task =
    fn() {
      wt.SENDMESSAGE(tmp, state.sender_stream_box)
      |> actor.send(state.session_subject, _)
    }
    |> task.async()

  case task.try_await(retry_task, 5000) {
    Ok(_) -> actor.continue(state)
    Error(_) -> {
      case list.length(state.bin) >= 256 {
        True -> actor.continue(wt.BinState(..state, bin: []))
        False -> {
          let new_msg = msg.Message(..tmp, message_code: msg.FAILEDTEMP)
          let new_bin = list.prepend(state.bin, new_msg)
          let new_state = wt.BinState(..state, bin: new_bin)
          actor.continue(new_state)
        }
      }
    }
  }
}

fn new(
  arg: wt.BinState,
  parent: process.Subject(process.Subject(wt.Retry)),
) -> Result(process.Subject(wt.Retry), actor.StartError) {
  actor.start_spec(actor.Spec(
    init: fn() {
      let worker_subj = process.new_subject()
      process.send(parent, worker_subj)

      process.new_selector()
      |> process.selecting(worker_subj, function.identity)
      |> actor.Ready(arg, _)
    },
    init_timeout: 1000,
    loop: bin_handler,
  ))
}
