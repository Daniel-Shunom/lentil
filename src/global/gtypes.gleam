import gleam/time/timestamp.{type Timestamp}

pub type LentilTimeStamp {
  LentilTimeStamp(time: Timestamp)
  Time(String)
}

pub type DB {
  DB
}
