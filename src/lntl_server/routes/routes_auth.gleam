import gleam/dynamic/decode
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import global/functions.{id_generator}
import lntl_server/lntl_workers/toolkit/worker_functions as wf
import lntl_server/lntl_workers/toolkit/worker_types as wt
import lntl_server/lntl_workers/w_users/u_session as us
import users/types/users.{type User}
import wisp

pub fn handle_auth_signin(req: wisp.Request) -> wisp.Response {
  use <- wisp.require_content_type(req, "application/json")
  use json <- wisp.require_json(req)
  case decode.run(json, credentials_decoder()) {
    Error(_) -> wisp.response(400)
    Ok(Credentials(uname, pswd)) -> {
      case is_user(uname, pswd) {
        None -> wisp.response(404)
        Some(valid_user) -> {
          let day = 60 * 60 * 24
          case wf.create_user_process(valid_user) {
            Error(_) -> wisp.response(500)
            Ok(#(user_subject, sessionid)) -> {
              let supervisor = us.start_user_supervisor()
              let msg = fn(reply_to) {
                us.START(valid_user, user_subject, sessionid, reply_to)
              }
              case actor.call(supervisor, msg, 20_000) {
                // NOTE -> instead of an error message, we let the user check 
                // through but ensure they have a process going on when they
                // joining a room, or we retry in the background.
                wt.FAILURE(_) -> wisp.response(500)
                wt.SUCCESS(_) -> {
                  wisp.response(200)
                  |> wisp.set_cookie(
                    req,
                    "session",
                    sessionid,
                    wisp.Signed,
                    day,
                  )
                }
                _ -> wisp.response(500)
              }
            }
          }
        }
      }
    }
  }
}

pub fn handle_auth_signout(req: wisp.Request) -> wisp.Response {
  case wisp.get_cookie(req, "session", wisp.Signed) {
    Error(_) -> wisp.response(200)
    Ok(session_id) -> {
      let err_msg = "Failure to close session"
      let supervisor = us.start_user_supervisor()
      let func = fn(reply_to) {
        us.STOP(session_id: session_id, reply_to: reply_to)
      }
      case actor.call(supervisor, func, 3000) {
        wt.SUCCESS(_) -> wisp.response(200)
        wt.FAILURE(_) -> {
          wisp.log_error(err_msg)
          wisp.response(200)
        }
        _ -> wisp.response(200)
      }
    }
  }
}

fn is_user(user_name _nm: String, password _pswd: String) -> Option(User) {
  todo
  option.Some(users.User(
    users.Name(first: "default", last: "default"),
    user_id: users.UserId(id: id_generator("lntl_user")),
    username: users.UserName(username: "default"),
    user_auth: True,
    user_dob: users.DOB(day: 15, month: 6, year: 2002),
    user_gender: users.CisGender,
    user_pronouns: users.Pronouns(singular: "He", plural: "Him"),
  ))
}

pub type Credentials {
  Credentials(username: String, password: String)
}

pub fn credentials_decoder() -> decode.Decoder(Credentials) {
  use username <- decode.field("username", decode.string)
  use password <- decode.field("password", decode.string)
  decode.success(Credentials(username, password))
}

pub fn parse_credentials(
  json_str: String,
) -> Result(Credentials, json.DecodeError) {
  json.parse(from: json_str, using: credentials_decoder())
}
