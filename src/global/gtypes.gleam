import gleam/time/calendar.{utc_offset}
import gleam/time/timestamp.{type Timestamp}

pub type LentilTimeStamp {
  LentilTimeStamp(time: Timestamp)
}
