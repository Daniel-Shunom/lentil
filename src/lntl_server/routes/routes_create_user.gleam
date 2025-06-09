import gleam/int
import gleam/list
import gleam/otp/task
import gleam/result
import gleam/string
import global/functions.{connect_lentildb, hasher}
import lntl_server/sql
import users/methods/methods.{create_user}
import users/types/users
import wisp

pub fn handle_create_user(req: wisp.Request) -> wisp.Response {
  use form <- wisp.require_form(req)
  case form.values {
    [
      #("first name", fname),
      #("last name", lname),
      #("password", pswd),
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
      case register_user(user, pswd) {
        Error(_) -> wisp.response(500)
        Ok(_) -> wisp.response(200)
      }
    }
    _ -> wisp.bad_request()
  }
}

fn register_user(user: users.User, pswd: String) {
  let handler = fn() { create_new_user(user, pswd) }
  task.try_await(task.async(handler), 10_000)
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

fn get_pronouns(pronouns: users.Pronouns) -> String {
  let users.Pronouns(singular, plural) = pronouns
  list.map([singular, plural], string.trim)
  |> string.join(with: "/")
}

fn get_gender_str(gender: users.Gender) -> String {
  case gender {
    users.CisGender -> "cisgender"
    users.TransGender -> "transgender"
    users.NonBinary -> "non-binary"
    users.Other -> "not specified"
  }
}

fn create_new_user(
  user: users.User,
  pswd: String,
) -> Result(CreateMsg, CreateMsg) {
  let err = "ERROR: FAILED TO CREATE USER"
  let connection = connect_lentildb()
  let users.UserId(id) = user.user_id
  let users.Name(fname, lname) = user.name
  let users.UserName(uname) = user.username
  let hashedp = hasher(pswd)
  let hashedu = hasher(uname)
  let users.DOB(day, month, year) = user.user_dob
  let gender = get_gender_str(user.user_gender)
  let pronouns = get_pronouns(user.user_pronouns)
  let response =
    sql.create_user(
      connection,
      id,
      fname,
      lname,
      hashedu,
      hashedp,
      day,
      month,
      year,
      gender,
      pronouns,
    )
  case response {
    Ok(_) -> Ok(SUCCESS)
    Error(_) -> Error(ERROR(err))
  }
}

type CreateMsg {
  SUCCESS
  ERROR(String)
}
