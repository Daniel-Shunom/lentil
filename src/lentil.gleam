import gleam/erlang/process
import gleam/io

pub fn main() {
  io.println("Hello from lentil!")

  process.sleep_forever()
}
