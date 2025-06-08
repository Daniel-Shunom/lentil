import gleam/int
import gleam/otp/task
import gleam/result
import gleam/string
import users/methods/methods.{create_user}
import users/types/users
import wisp

pub fn handle_create_user(req: wisp.Request) -> wisp.Response {
  use form <- wisp.require_form(req)
  case form.values {
    [
      #("first name", fname),
      #("last name", lname),
      #("user name", uname),
      #("pronouns singular", ps),
      #("pronouns plural", pp),
      #("day", day),
      #("month", month),
      #("year", year),
      #("gender", gender),
    ] -> {
      let user =
        create_user(
          first_name: fname,
          last_name: lname,
          username: uname,
          pronouns_singular: ps,
          pronouns_plural: pp,
          dob_day: get_num(day),
          dob_month: get_num(month),
          dob_year: get_num(year),
          user_gender: get_gender(gender),
        )
      case register_user(user) {
        Error(_) -> wisp.response(500)
        Ok(_) -> wisp.response(200)
      }
    }
    _ -> wisp.bad_request()
  }
}

fn register_user(user: users.User) {
  let handler = fn() { create_new_user(user) }
  task.try_await(task.async(handler), 10_000)
  |> result.unwrap(Error(Nil))
}

fn get_num(str: String) -> Int {
  result.unwrap(int.parse(str), 0)
}

fn get_gender(str: String) -> users.Gender {
  case string.lowercase(string.trim(str)) {
    "cis" <> _ -> users.CisGender
    "trans" <> _ -> users.TransGender
    "non" <> _ -> users.NonBinary
    _ -> users.Other
  }
}

pub fn create_new_user(user: users.User) -> Result(CreateMsg, Nil) {
  todo as "handle user creation logic here"
}

pub type CreateMsg {
  SUCCESS
  ERROR(String)
}
