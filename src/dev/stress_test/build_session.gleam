import gleam/dynamic
import gleam/dynamic/decode
import dev/stress_test/build_room
import dev/stress_test/build_user
import gleam/result
import gleam/httpc

pub fn build_session(first, last) {
  use resp <- result.try(build_user.build_user(first, last))
  let dec = {
    use devuser <- result.try(decode.run(
      dynamic.string(resp.body),
      build_user.dev_userdecoder()
    ))
    Ok(devuser)
  } |> result.map_error(fn(_) { httpc.ResponseTimeout })
  use resp2 <- result.try(dec)
  let _ = build_user.login_devuser(resp2)
  use resp3 <- result.try(build_room.build_room(resp2.userid))
  let dec2 = {
    use devroom <- result.try(decode.run(
      dynamic.string(resp3.body),
      build_room.dev_roomdecoder()
    ))
    Ok(devroom)
  } |> result.map_error(fn(_) { httpc.ResponseTimeout })
  use resp4 <- result.try(dec2)
  Ok(DevSession(resp2.userid, resp4.id))
}

pub type DevSession {
  DevSession(userid: String, roomid: String)
}
