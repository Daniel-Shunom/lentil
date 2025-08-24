import birl
import birl/duration
import gleam/bool
import gleam/result
import global/functions.{get_env}
import gwt

pub opaque type LentilJWT(client_typej) {
  LentilJWT(token: String)
}

pub type LentilVerified {
  LentilVerified
}

pub type Mobile

pub type Browser

pub fn set_jwt_mobile(id: String) -> LentilJWT(Mobile) {
  let time =
    duration.months(3)
    |> birl.add(birl.now(), _)
    |> birl.to_unix()

  gwt.new()
  |> gwt.set_audience(get_env("JWT_AUD"))
  |> gwt.set_expiration(time)
  |> gwt.set_jwt_id(id)
  |> gwt.set_issued_at(
    birl.now()
    |> birl.to_unix(),
  )
  |> gwt.to_signed_string(gwt.HS256, get_env("JWT_SECRET"))
  |> LentilJWT
}

pub fn get_jwt_mobile(jwt: LentilJWT(Mobile)) -> String {
  jwt.token
}

pub fn set_jwt_browser(id: String) -> LentilJWT(Browser) {
  let time =
    duration.hours(3)
    |> birl.add(birl.now(), _)
    |> birl.to_unix()

  gwt.new()
  |> gwt.set_audience(get_env("JWT_AUD"))
  |> gwt.set_expiration(time)
  |> gwt.set_jwt_id(id)
  |> gwt.set_issued_at(
    birl.now()
    |> birl.to_unix(),
  )
  |> gwt.to_signed_string(gwt.HS256, get_env("JWT_SECRET"))
  |> LentilJWT
}

pub fn get_jwt_browser(jwt: LentilJWT(Browser)) -> String {
  jwt.token
}

pub fn validate_jwt(token: String, id: String) -> Result(LentilVerified, Nil) {
  let secret = get_env("JWT_SECRET")
  let aud = get_env("JWT_AUD")
  let now =
    birl.now()
    |> birl.to_unix()
  use verified <- result.try(
    gwt.from_signed_string(token, secret)
    |> result.replace_error(Nil),
  )
  use audience <- result.try(
    gwt.get_audience(verified)
    |> result.replace_error(Nil),
  )
  use expiration <- result.try(
    gwt.get_expiration(verified)
    |> result.replace_error(Nil),
  )
  use userid <- result.try(
    gwt.get_jwt_id(verified)
    |> result.replace_error(Nil),
  )
  use issuetime <- result.try(
    gwt.get_issued_at(verified)
    |> result.replace_error(Nil),
  )
  use <- bool.guard(now - issuetime > expiration, Error(Nil))
  use <- bool.guard(id != userid, Error(Nil))
  use <- bool.guard(audience != aud, Error(Nil))
  Ok(LentilVerified)
}
