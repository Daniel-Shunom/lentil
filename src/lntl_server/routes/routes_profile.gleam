import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option
import gleam/string
import global/ctx/ctx
import lntl_server/sql
import wisp

pub fn handle_get_profile(req: wisp.Request, ctx: ctx.Context) -> wisp.Response {
  use <- wisp.require_content_type(req, "application/json")
  use json <- wisp.require_json(req)
  case decode.run(json, id_decoder()) {
    Error(_) -> wisp.response(500)
    Ok(userid) -> {
      case sql.fetch_user_by_id(ctx.db_connection, userid) {
        Error(_) -> wisp.response(400)
        Ok(user) -> {
          case user.rows {
            [] -> wisp.response(400)
            [val, ..] -> {
              let userid = #("userid", json.string(val.id))
              let name = #(
                "name",
                json.string(val.first_name <> " " <> val.last_name),
              )
              let username = #("username", json.string(val.username))
              let dob = #(
                "dob",
                [val.dob_day, val.dob_month, val.dob_year]
                  |> list.map(int.to_string)
                  |> string.join("/")
                  |> json.string(),
              )
              let gender = #("gender", json.string(val.gender))
              let pronouns = #("pronouns", json.string(val.pronouns))
              let auth = #(
                "authenticated",
                val.is_authenticated
                  |> option.unwrap(False)
                  |> json.bool(),
              )
              [userid, name, username, dob, gender, pronouns, auth]
              |> json.object()
              |> json.to_string_tree()
              |> wisp.json_response(200)
            }
          }
        }
      }
    }
  }
}

fn id_decoder() -> decode.Decoder(String) {
  use userid <- decode.field("userid", decode.string)
  decode.success(userid)
}
