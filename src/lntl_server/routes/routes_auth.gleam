import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/string
import global/ctx/ctx
import global/functions.{hasher}
import lntl_server/sql
import users/types/users.{type User, User}
import wisp

pub fn handle_auth_signin(req: wisp.Request, ctx: ctx.Context) -> wisp.Response {
  use <- wisp.require_content_type(req, "application/json")
  use json <- wisp.require_json(req)
  case decode.run(json, credentials_decoder()) {
    Error(_) -> wisp.response(400)
    Ok(Credentials(uname, pswd)) -> {
      case is_user(uname, pswd, ctx) {
        None -> wisp.response(404)
        Some(valid_user) -> {
          let _day = 60 * 60 * 24
          ctx.ADD(valid_user)
          |> actor.send(ctx.supbox, _)
          wisp.response(200)
        }
      }
    }
  }
}

pub fn handle_auth_signout(req: wisp.Request, ctx: ctx.Context) -> wisp.Response {
  use <- wisp.require_content_type(req, "application/json")
  use json <- wisp.require_json(req)
  case decode.run(json, id_decoder()) {
    Error(_) -> wisp.bad_request()
    Ok(userid) -> {
      ctx.REM(userid)
      |> actor.send(ctx.supbox, _)
      wisp.response(200)
    }
  }
}

fn is_user(
  user_name unm: String,
  password pswd: String,
  context ctx: ctx.Context,
) -> Option(User) {
  let hashed_p = hasher(pswd)
  let hashed_u = hasher(unm)
  case sql.fetch_user(ctx.db_connection, hashed_p, hashed_u) {
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

fn id_decoder() -> decode.Decoder(String) {
  use userid <- decode.field("userid", decode.string)
  decode.success(userid)
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
