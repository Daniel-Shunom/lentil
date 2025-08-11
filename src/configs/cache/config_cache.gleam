import gleam/option.{type Option, None}
import gleam/erlang/process.{type Subject}
import configs/configs.{type CacheConfig,CacheConfig}

pub fn cacheconfig_new() -> CacheConfig(a, b) {
  CacheConfig(
    provider: fn() { Nil },
    channel: None,
    handler: None
  )
}

pub fn cacheconfig_set_provider(
  config: CacheConfig(a, b),
  provider: fn() -> Nil,
) -> CacheConfig(a, b) {
  CacheConfig(..config, provider: provider)
}

pub fn cacheconfig_set_channel(
  config: CacheConfig(a, b),
  channel: Option(Subject(b))
) -> CacheConfig(a, b) {
  CacheConfig(..config, channel: channel)
}

pub fn cacheconfig_set_handler(
  config: CacheConfig(a, b),
  handler: Option(fn(b, Subject(b)) -> Nil)
) -> CacheConfig(a, b) {
  CacheConfig(..config, handler: handler)
}
