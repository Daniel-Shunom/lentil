import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/string
import global/functions.{connect_lentildb, hasher}
import lntl_server/lntl_workers/toolkit/worker_functions as wf
import lntl_server/lntl_workers/toolkit/worker_types as wt
import lntl_server/lntl_workers/w_users/u_session as us
import lntl_server/sql
import users/types/users.{type User, User}
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

fn is_user(user_name unm: String, password pswd: String) -> Option(User) {
  let hashed_p = hasher(pswd)
  let hashed_u = hasher(unm)
  case sql.fetch_user(connect_lentildb(), hashed_p, hashed_u) {
    Error(_) -> None
    Ok(query_res) -> {
      case list.first(query_res.rows) {
        Error(_) -> None
        Ok(res) -> {
          let pronouns = pnouns(res.pronouns)
          Some(User(
            name: users.Name(res.first_name, res.last_name),
            user_id: users.UserId(res.id),
            username: users.UserName(res.username),
            user_auth: True,
            user_dob: users.DOB(res.dob_day, res.dob_month, res.dob_year),
            user_gender: get_gender(res.gender),
            user_pronouns: users.Pronouns(pronouns.0, pronouns.1),
          ))
        }
      }
    }
  }
}

type Credentials {
  Credentials(username: String, password: String)
}

fn credentials_decoder() -> decode.Decoder(Credentials) {
  use username <- decode.field("username", decode.string)
  use password <- decode.field("password", decode.string)
  decode.success(Credentials(username, password))
}

fn get_gender(str: String) -> users.Gender {
  case string.lowercase(string.trim(str)) {
    "cis" <> _ -> users.CisGender
    "trans" <> _ -> users.TransGender
    "non" <> _ -> users.NonBinary
    _ -> users.Other
  }
}

fn pnouns(str: String) -> #(String, String) {
  case string.split(str, "/") {
    [sing, plur, ..] -> #(sing, plur)
    _ -> #("", "")
  }
}
