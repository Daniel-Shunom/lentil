import gleam/option
import gleam/string
import gleam/erlang/process
import gleam/int
import gleam/otp/actor
import global/functions
import lntl_frontline/msg_types as mt
import messages/types/msg

pub type OsmonMsg {
  OsmonMsg(
    total_mem: Int, 
    used_mem: Int, 
    cpu_load: Int,
    resp: process.Subject(OsmonMsg)
  )
}

pub fn start_osmon(subj: process.Subject(mt.GlobalMonitorMessage(msg.Message))) {
  let osmon_handler = fn(msg, state) { handler(msg, state, subj) }
  let assert Ok(osmon_subj) = actor.start(option.None, osmon_handler)
  let _ = poll(osmon_subj)
  osmon_subj
}

fn handler(
  message: OsmonMsg,
  state: option.Option(process.Subject(OsmonMsg)),
  subj: process.Subject(mt.GlobalMonitorMessage(msg.Message)),
) -> actor.Next(OsmonMsg, option.Option(process.Subject(OsmonMsg))) {
  case state {
    option.None -> {
      let new_state = message.resp
      // let 
      actor.continue(option.Some(new_state))
    }
    option.Some(replsubj) -> {
      let OsmonMsg(tmem, umem, _load, _) = message
      let load = int.to_float(umem / tmem * 10)
      let to_monitor =
        mt.ServerRouterMessage(mt.SERVERNodeLoad(
          load,
          load,
          functions.get_timestamp(),
        ))
      // let new_msg = create_osmon(replsubj)
      poll(replsubj)
      actor.send(subj, to_monitor)
      actor.continue(state)
    }
  }
}

pub fn poll(subj) {
  let msg = fn() {
    echo "SENDING PERIODICALLY TO " <> string.inspect(subj)
    OsmonMsg(0, 0, 0, subj)
  }
  process.send_after(subj, 300, msg())
}

fn create_osmon(subj) {
  OsmonMsg(0, 0, 0, subj)
}

@external(erlang, "osmon_gs", "ffi_start")
pub fn ffi_osmon(subj: process.Pid, interval: Int) -> a

@external(erlang, "observer", "start")
pub fn observer() -> a

@external(erlang, "sys_utils", "get_mem")
fn getmem() -> #(Int, Int)

@external(erlang, "sys_utils", "get_load")
fn getload() -> Int

@external(erlang, "sys_utils", "start_osmon")
fn startosmon() -> Result(a, a)