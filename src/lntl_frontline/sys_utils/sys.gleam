import global/functions
import gleam/int
import messages/types/msg
import gleam/erlang/process
import gleam/otp/actor
import lntl_frontline/msg_types as mt

pub opaque type OsmonMsg {
  OsmonMsg(total_mem: Int, used_mem: Int, cpu_load: Int)
}

pub fn start_osmon(subj: process.Subject(mt.GlobalMonitorMessage(msg.Message))) {
  let osmon_handler = fn(msg, state) {
    handler(msg, state, subj)
  }
  let assert Ok(osmon_subj) = actor.start(Nil, osmon_handler)
  let _ = ffi_osmon(osmon_subj, 300)
  osmon_subj
}

fn handler(message: OsmonMsg, state: Nil, subj: process.Subject(mt.GlobalMonitorMessage(msg.Message))) -> actor.Next(OsmonMsg, Nil) {
  let OsmonMsg(tmem, umem, _load) = message
  let load =  int.to_float(umem / tmem * 100)
  let to_monitor  = 
    mt.ServerRouterMessage(mt.SERVERNodeLoad(load, load, functions.get_timestamp()))
  actor.send(subj, to_monitor)
  actor.continue(state)
}

@external(erlang, "osmon_gs", "ffi_start")
pub fn ffi_osmon(subj: process.Subject(OsmonMsg), interval: Int) -> process.Pid