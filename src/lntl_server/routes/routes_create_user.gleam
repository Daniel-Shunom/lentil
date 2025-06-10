import gleam/dynamic/decode
import gleam/list
import gleam/otp/task
import gleam/string
import global/functions.{connect_lentildb, hasher}
import lntl_server/sql
import users/methods/methods.{create_user}
import users/types/users
import wisp

pub fn handle_create_user(req: wisp.Request) -> wisp.Response {
  use <- wisp.require_content_type(req, "application/json")
  use json <- wisp.require_json(req)

  case decode.run(json, user_decoder()) {
    Error(_) -> wisp.response(400)
    Ok(UserReqs(
      firstname,
      lastname,
      username,
      password,
      pronounssingular,
      pronounsplural,
      day,
      month,
      year,
      gender_str,
    )) -> {
      let user =
        create_user(
          first_name: firstname,
          last_name: lastname,
          username: username,
          pronouns_singular: pronounssingular,
          pronouns_plural: pronounsplural,
          dob_day: day,
          dob_month: month,
          dob_year: year,
          user_gender: get_gender(gender_str),
        )
      case register_user(user, password) {
        Error(_) -> wisp.response(500)
        Ok(results) -> {
          case results {
            Error(msg) -> {
              echo msg
              wisp.response(500)
            }
            Ok(msg) -> {
              echo msg
              wisp.response(200)
            }
          }
        }
      }
    }
  }
}

fn user_decoder() -> decode.Decoder(UserReqs) {
  use firstname <- decode.field("firstname", decode.string)
  use lastname <- decode.field("lastname", decode.string)
  use username <- decode.field("username", decode.string)
  use password <- decode.field("password", decode.string)
  use pronounssingular <- decode.field("pronounssingular", decode.string)
  use pronounsplural <- decode.field("pronounsplural", decode.string)
  use day <- decode.field("day", decode.int)
  use month <- decode.field("month", decode.int)
  use year <- decode.field("year", decode.int)
  use gender_str <- decode.field("gender", decode.string)
  decode.success(UserReqs(
    firstname,
    lastname,
    username,
    password,
    pronounssingular,
    pronounsplural,
    day,
    month,
    year,
    gender_str,
  ))
}

type UserReqs {
  UserReqs(
    firstname: String,
    lastname: String,
    username: String,
    password: String,
    pronounssingular: String,
    pronounsplural: String,
    day: Int,
    month: Int,
    year: Int,
    gender_str: String,
  )
}

// ————————————————————————————————————————————
// The rest of your helpers stay the same:

fn register_user(user: users.User, pswd: String) {
  let handler = fn() { create_new_user(user, pswd) }
  task.try_await(task.async(handler), 10_000)
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
  let users.Pronouns(s, p) = pronouns
  list.map([s, p], string.trim)
  |> string.join(with: "/")
}

fn get_gender_str(g: users.Gender) -> String {
  case g {
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
  let conn = connect_lentildb()
  let users.UserId(id) = user.user_id
  let users.Name(f, l) = user.name
  let users.UserName(un) = user.username
  let hashedp = hasher(pswd)
  let hashedu = hasher(un)
  let users.DOB(d, m, y) = user.user_dob
  let gender = get_gender_str(user.user_gender)
  let pronouns = get_pronouns(user.user_pronouns)
  let response =
    sql.create_user(conn, id, f, l, hashedu, hashedp, d, m, y, gender, pronouns)
  case response {
    Ok(_) -> Ok(SUCCESS)
    Error(_) -> Error(ERROR(err))
  }
}

type CreateMsg {
  SUCCESS
  ERROR(String)
}
