import gleam/erlang/process.{type Subject}
import gleam/option.{type Option}

pub type DbConfig {
  DbConfig(
    host: Int,
    port: Int,
    poolsize: Int,
    username: String,
    password: String,
  )
}

pub type CacheConfig(cache_provider, msg_type) {
  CacheConfig(
    provider: fn() -> Nil,
    channel: Option(Subject(msg_type)),
    handler: Option(fn(msg_type, Subject(msg_type)) -> Nil),
  )
}

pub type LogConfig(log_provider, msg_type) {
  LogConfig(
    provider: fn() -> Nil,
    channel: Option(Subject(msg_type)),
    handler: Option(fn(msg_type, Subject(msg_type)) -> Nil),
  )
}

pub type AuthConfig(auth_provider, msg_type) {
  AuthConfig(
    provider: fn() -> Nil,
    channel: Option(Subject(msg_type)),
    handler: Option(fn(msg_type, Subject(msg_type)) -> Nil),
  )
}

pub type BucketConfig(bucket_provider, msg_type) {
  BucketConfig(
    provider: fn() -> Nil,
    channel: Option(Subject(msg_type)),
    handler: Option(fn(msg_type, Subject(msg_type)) -> Nil),
  )
}
