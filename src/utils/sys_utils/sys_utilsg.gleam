import gleam/io
import gleam/option
import gleam/string
import gleam/erlang/process
import gleam/int
import gleam/otp/actor
import global/functions
import utils/msg_types as mt
import models/messages/types/msg

pub type OsmonMsg {
  OsmonMsg(
    total_mem: Int, 
    used_mem: Int, 
    cpu_load: Int,
    resp: process.Subject(OsmonMsg)
  )
}

pub fn start_osmon(subj: process.Subject(mt.GlobalMonitorMessage(msg.Message))) {
  let _ = initialize_osmon()
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
      poll(message.resp)
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

fn poll(subj) {
  process.send_after(subj, 300, fn() {
    let #(total, used) = getmem()
    let load = getload()
    io.println(
    "Total: " <> string.inspect(total) <> "\n"
    <> "Used: " <> string.inspect(used) <> "\n"
    <> "Load: " <> string.inspect(load) <> "\n\n"
    ) 
    OsmonMsg(total, used,  load, subj)
  }())
}



@external(erlang, "observer", "start")
pub fn observer() -> a

@external(erlang, "sys_utils", "get_mem")
fn getmem() -> #(Int, Int)

@external(erlang, "sys_utils", "get_load")
fn getload() -> Int

@external(erlang, "sys_utils", "start_osmon")
fn initialize_osmon() -> a
