import gleam/io
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn hello_world_test() {
  io.print("helo world")
  |> should.equal(Nil)
}
