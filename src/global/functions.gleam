import gleam/int
import gleam/time/calendar.{utc_offset}
import gleam/time/duration.{to_seconds_and_nanoseconds}
import gleam/time/timestamp.{from_unix_seconds_and_nanoseconds}
import global/gtypes.{type LentilTimeStamp}
import prng/random
import prng/seed

// NOTE -> this gets a universal epoch timestamp
pub fn get_timestamp() -> LentilTimeStamp {
  let #(seconds, nano_seconds) = to_seconds_and_nanoseconds(utc_offset)
  gtypes.LentilTimeStamp(time: from_unix_seconds_and_nanoseconds(
    seconds,
    nano_seconds,
  ))
}

pub fn id_generator(prefix: String) -> String {
  let str = random.fixed_size_string(32)
  let num = random.int(0, 100_000)
  let secure_prefix =
    random.int(0, 13)
    |> random.random_sample()
    |> seed.new()
    |> random.sample(num, _)
    |> int.to_string()

  let secure_id =
    random.int(0, 9)
    |> random.random_sample()
    |> seed.new()
    |> random.sample(str, _)

  prefix <> secure_prefix <> secure_id
}
