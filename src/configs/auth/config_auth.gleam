import configs/configs.{type AuthConfig, AuthConfig}
import gleam/erlang/process.{type Subject}
import gleam/option.{type Option, None}

pub fn authconfig_new() -> AuthConfig(a, b) {
  AuthConfig(provider: fn() { Nil }, channel: None, handler: None)
}

pub fn authconfig_set_provider(
  config: AuthConfig(a, b),
  provider: fn() -> Nil,
) -> AuthConfig(a, b) {
  AuthConfig(..config, provider: provider)
}

pub fn authconfig_set_channel(
  config: AuthConfig(a, b),
  channel: Option(Subject(b)),
) -> AuthConfig(a, b) {
  AuthConfig(..config, channel: channel)
}

pub fn authconfig_set_handler(
  config: AuthConfig(a, b),
  handler: Option(fn(b, Subject(b)) -> Nil),
) -> AuthConfig(a, b) {
  AuthConfig(..config, handler: handler)
}
