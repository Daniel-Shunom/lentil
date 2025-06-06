import gleam/time/calendar.{utc_offset}
import gleam/time/duration.{to_seconds_and_nanoseconds}
import gleam/time/timestamp.{from_unix_seconds_and_nanoseconds}
import global/gtypes.{type LentilTimeStamp}

// NOTE -> this gets a universal epoch timestamp
pub fn get_timestamp() -> LentilTimeStamp {
  let #(seconds, nano_seconds) = to_seconds_and_nanoseconds(utc_offset)
  gtypes.LentilTimeStamp(time: from_unix_seconds_and_nanoseconds(
    seconds,
    nano_seconds,
  ))
}
