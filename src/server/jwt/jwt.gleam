import gwt
import global/functions.{get_env}
import birl
import birl/duration

pub opaque type LentilJWT(client_typej) {
  LentilJWT(token: String)
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
    |> birl.to_unix()
  )
  |> gwt.to_signed_string(
    gwt.HS256,
    get_env("JWT_SECRET")
  ) 
  |> LentilJWT     
}

pub fn set_jwt_browser(id: String) -> LentilJWT(Browser) {
  let time =
    duration.hours(3)
    |> birl.add(birl.now(), _)
    |> birl.to_unix()

  gwt.new()
  |> gwt.set_audience(get_env("JWTP_AUD"))
  |> gwt.set_expiration(time)
  |> gwt.set_jwt_id(id)
  |> gwt.set_issued_at(
    birl.now()
    |> birl.to_unix()
  )
  |> gwt.to_signed_string(
    gwt.HS256,
    get_env("JWT_SECRET")
  )
  |> LentilJWT
}
