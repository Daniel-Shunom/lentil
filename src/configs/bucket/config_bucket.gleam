import gleam/erlang/process.{type Subject}
import gleam/option.{type Option, None}

pub type BucketConfig(bucket_provider, msg_type) {
  BucketConfig(
    provider: fn() -> Nil,
    channel: Option(Subject(msg_type)),
    handler: Option(fn(msg_type, Subject(msg_type)) -> Nil),
  )
}

pub fn configbucket_new() -> BucketConfig(a, b) {
  BucketConfig(provider: fn() { Nil }, channel: None, handler: None)
}

pub fn configbucket_set_provider(
  config: BucketConfig(a, b),
  provider: fn() -> Nil,
) -> BucketConfig(a, b) {
  BucketConfig(..config, provider: provider)
}

pub fn configbucket_set_channel(
  config: BucketConfig(a, b),
  channel: Option(Subject(b)),
) -> BucketConfig(a, b) {
  BucketConfig(..config, channel: channel)
}

pub fn configbucket_set_handler(
  config: BucketConfig(a, b),
  handler: Option(fn(b, Subject(b)) -> Nil),
) -> BucketConfig(a, b) {
  BucketConfig(..config, handler: handler)
}
