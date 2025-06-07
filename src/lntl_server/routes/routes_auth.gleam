import gleam/option.{None, Some}
import gleam/otp/actor
import gleam/string
import global/functions.{is_user}
import lntl_server/lntl_workers/toolkit/worker_functions as wf
import lntl_server/lntl_workers/toolkit/worker_types as wt
import lntl_server/lntl_workers/w_users/u_session as us
import wisp

pub fn handle_auth_signin(req: wisp.Request) -> wisp.Response {
  use form <- wisp.require_form(req)
  case form.values {
    [#("username", username), #("password", password)] -> {
      let utrim = string.trim(username)
      let ptrim = string.trim(password)
      case is_user(utrim, ptrim) {
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
              case actor.call(supervisor, msg, 2000) {
                wt.FAILURE(_) -> wisp.response(500)
                wt.SUCCESS(_) -> {
                  wisp.redirect("/")
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
    _ -> wisp.bad_request()
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
