import gleam/bool
import gleam/dynamic/decode
import gleam/http/request
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import gleam/string
import global/ctx/ctx
import global/ctx/types as t
import global/functions.{hasher}
import models/users/types/users.{type User, User}
import server/cache/cache_user
import server/jwt/jwt
import server/sql
import utils/msg_types as mt
import wisp

pub fn handle_auth_signin(req: wisp.Request, ctx: ctx.Context) -> wisp.Response {
  use <- wisp.require_content_type(req, "application/json")
  use json <- wisp.require_json(req)
  let err = wisp.response(400)
  {
    use Credentials(uname, paswd) <- result.try(
      decode.run(json, credentials_decoder())
      |> result.replace_error(err),
    )
    use user <- result.try(
      is_user(uname, paswd, ctx)
      |> option.to_result(err),
    )
    let userid = #("userid", json.string(user.user_id.id))
    let auth = #("authenticated", json.bool(user.user_auth))
    let token = #(
      "token",
      {
        let webtoken =
          jwt.set_jwt_browser(user.user_id.id)
          |> jwt.get_jwt_browser()
        use header <- result.try(
          request.get_header(req, "user-agent")
          |> result.replace_error(json.string(webtoken)),
        )
        use <- bool.guard(
          !string.contains(header, "mobile"),
          Ok(json.string(webtoken)),
        )
        jwt.set_jwt_mobile(user.user_id.id)
        |> jwt.get_jwt_mobile()
        |> json.string()
        |> Ok
      }
        |> result.unwrap_both(),
    )

    let message =
      mt.ClientRouterMessage(mt.CLIENTAuthEvent(
        userid: user.user_id.id,
        event_type: mt.SIGNIN,
        success: True,
        lntl_time: functions.get_timestamp(),
      ))

    cache_user.cache_user(ctx.user_cache, user)
    actor.send(ctx.server_monitor, message)
    t.ADD(user)
    |> actor.send(ctx.usersupbox, _)
    list.new()
    |> list.prepend(token)
    |> list.prepend(auth)
    |> list.prepend(userid)
    |> json.object()
    |> json.to_string_tree()
    |> wisp.json_response(200)
    |> Ok
  }
  |> result.unwrap_both()
}

/// This is quite unsafe for now, change this later
pub fn handle_auth_signout(req: wisp.Request, ctx: ctx.Context) -> wisp.Response {
  use <- wisp.require_content_type(req, "application/json")
  use json <- wisp.require_json(req)
  let err = wisp.response(400)
  {
    use id <- result.try(
      decode.run(json, id_decoder())
      |> result.replace_error(err),
    )
    let message =
      mt.ClientRouterMessage(mt.CLIENTAuthEvent(
        userid: id,
        event_type: mt.SIGNOUT,
        success: True,
        lntl_time: functions.get_timestamp(),
      ))
    cache_user.uncache_user(ctx.user_cache, id)
    actor.send(ctx.server_monitor, message)
    actor.send(ctx.usersupbox, t.REM(id))
    wisp.response(200)
    |> Ok
  }
  |> result.unwrap_both()
}

fn is_user(
  user_name unm: String,
  password pswd: String,
  context ctx: ctx.Context,
) -> Option(User) {
  let hashed_p = hasher(pswd)
  let hashed_u = hasher(unm)
  case sql.fetch_user(ctx.db_connection, hashed_u, hashed_p) {
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
