import gleam/bytes_tree
import gleam/erlang/process
import gleam/option.{None}
import gleam/otp/actor
import glisten.{Packet}

pub fn main() {
  let assert Ok(_) =
    glisten.handler(fn(_conn) { #(Nil, None) }, fn(msg, state, conn) {
      let assert Packet(msg) = msg
      let assert Ok(_) = glisten.send(conn, bytes_tree.from_bit_array(msg))
      actor.continue(state)
    })
    // NOTE:  By default, `glisten` will listen on the loopback interface.  If
    // you want to listen on all interfaces, pass the following.  You can also
    // specify other interface values, including IPv6 addresses.
    // |> glisten.bind("0.0.0.0")
    |> glisten.serve(3000)

  process.sleep_forever()
}
