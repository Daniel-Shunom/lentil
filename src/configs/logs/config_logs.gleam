import gleam/option.{type Option, None}
import gleam/erlang/process.{type Subject}
import configs/configs.{type LogConfig,LogConfig}

pub fn logconfig_new() -> LogConfig(a, b) {
  LogConfig(
    provider: fn() { Nil },
    channel: None, 
    handler: None,
  )
}

pub fn logconfig_set_provider(
  config: LogConfig(a, b),
  provider: fn() -> Nil
) -> LogConfig(a, b) {
  LogConfig(..config, provider: provider)
}

pub fn logconfig_set_channel(
  config: LogConfig(a, b),
  channel: Option(Subject(b))
) -> LogConfig(a, b) {
  LogConfig(..config, channel: channel)
}

pub fn logconfig_set_handler(
  config: LogConfig(a, b),
  handler: Option(fn(Subject(b)) -> Nil)
) -> LogConfig(a, b) {
  LogConfig(..config, handler: handler)
}
