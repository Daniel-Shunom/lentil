import gleam/option.{type Option}
import gleam/erlang/process.{type Subject}
import configs/bucket/config_bucket.{type BucketConfig}
import configs/cache/config_cache.{type CacheConfig}
import configs/auth/config_auth.{type AuthConfig}
import models/messages/types/msg.{type Message}
import configs/logs/config_logs.{type LogConfig}

pub type LentilOptionalConfigs {
  LentilOptionalConfigs(
    log_config: LogConfig(Nil, Message),
    auth_config: AuthConfig(Nil, Message),
    cache_config: CacheConfig(Nil, Message),
    bucket_config: BucketConfig(Nil, Message)
  )
}

pub fn set_log_config(
  config: LentilOptionalConfigs,
  log_config: LogConfig(Nil, Message)
) -> LentilOptionalConfigs {
  LentilOptionalConfigs(
    ..config,
    log_config: log_config
  )
}

pub fn set_auth_config(
  config: LentilOptionalConfigs,
  auth_config: AuthConfig(Nil, Message)
) -> LentilOptionalConfigs {
  LentilOptionalConfigs(
    ..config,
    auth_config: auth_config
  )
}

pub fn set_cache_config(
  config: LentilOptionalConfigs,
  cache_config: CacheConfig(Nil, Message)
) -> LentilOptionalConfigs {
  LentilOptionalConfigs(
    ..config,
    cache_config: cache_config
  )
}

pub fn set_bucket_config(
  config: LentilOptionalConfigs,
  bucket_config: BucketConfig(Nil, Message)
) -> LentilOptionalConfigs {
  LentilOptionalConfigs(
    ..config,
    bucket_config: bucket_config
  )
}

pub fn create_optional_lentil_config() -> LentilOptionalConfigs {
  LentilOptionalConfigs(
    log_config: config_logs.logconfig_new(),
    auth_config: config_auth.authconfig_new(),
    cache_config: config_cache.cacheconfig_new(),
    bucket_config: config_bucket.configbucket_new()
  )
}

pub fn set_log_config_provider(
  config: LentilOptionalConfigs, 
  provider: fn() -> Nil
) -> LentilOptionalConfigs {
  LentilOptionalConfigs(
    ..config,
    log_config: config_logs.logconfig_set_provider(
      config.log_config,
      provider
    )
  )
}

pub fn set_log_config_channel(
  config: LentilOptionalConfigs,
  channel: Option(Subject(Message)),
) -> LentilOptionalConfigs {
  LentilOptionalConfigs(
    ..config,
    log_config: config_logs.logconfig_set_channel(
      config.log_config,
      channel
    )
  )
}

pub fn set_log_config_handler(
  config: LentilOptionalConfigs,
  handler: Option(fn(Message, Subject(Message)) -> Nil)
) -> LentilOptionalConfigs {
  LentilOptionalConfigs(
    ..config,
    log_config: config_logs.logconfig_set_handler(
      config.log_config,
      handler
    )
  )
}

pub fn set_cache_config_provider(
  config: LentilOptionalConfigs,
  provider: fn() -> Nil
) -> LentilOptionalConfigs {
  LentilOptionalConfigs(
    ..config,
    cache_config: config_cache.cacheconfig_set_provider(
      config.cache_config,
      provider
    )
  )
}

pub fn set_cache_config_channel(
  config: LentilOptionalConfigs,
  channel: Option(Subject(Message)),
) -> LentilOptionalConfigs {
  LentilOptionalConfigs(
    ..config,
    cache_config: config_cache.cacheconfig_set_channel(
      config.cache_config,
      channel
    )
  )
}

pub fn set_cache_config_handler(
  config: LentilOptionalConfigs,
  handler: Option(fn(Message, Subject(Message)) -> Nil)
) -> LentilOptionalConfigs {
  LentilOptionalConfigs(
    ..config,
    cache_config: config_cache.cacheconfig_set_handler(
      config.cache_config,
      handler
    )
  )
}

pub fn set_auth_config_provider(
  config: LentilOptionalConfigs,
  provider: fn() -> Nil
) -> LentilOptionalConfigs {
  LentilOptionalConfigs(
    ..config,
    auth_config: config_auth.authconfig_set_provider(
      config.auth_config,
      provider
    )
  )
}

pub fn set_auth_config_channel(
  config: LentilOptionalConfigs,
  channel: Option(Subject(Message))
) -> LentilOptionalConfigs {
  LentilOptionalConfigs(
    ..config,
    auth_config: config_auth.authconfig_set_channel(
      config.auth_config,
      channel
    )
  )
}

pub fn set_auth_config_handler(
  config: LentilOptionalConfigs,
  handler: Option(fn(Message, Subject(Message)) -> Nil)
) -> LentilOptionalConfigs {
  LentilOptionalConfigs(
    ..config,
    auth_config: config_auth.authconfig_set_handler(
      config.auth_config,
      handler
    )
  )
}

pub fn set_bucket_config_provider(
  config: LentilOptionalConfigs,
  provider: fn() -> Nil
) -> LentilOptionalConfigs {
  LentilOptionalConfigs(
    ..config,
    bucket_config: config_bucket.configbucket_set_provider(
      config.bucket_config,
      provider
    )
  )
}

pub fn set_bucket_config_channel(
  config: LentilOptionalConfigs,
  channel: Option(Subject(Message))
) -> LentilOptionalConfigs {
  LentilOptionalConfigs(
    ..config,
    bucket_config: config_bucket.configbucket_set_channel(
      config.bucket_config,
      channel
    )
  )
}

pub fn set_bucket_config_handler(
  config: LentilOptionalConfigs,
  handler: Option(fn(Message, Subject(Message)) -> Nil)
) -> LentilOptionalConfigs {
  LentilOptionalConfigs(
    ..config,
    bucket_config: config_bucket.configbucket_set_handler(
      config.bucket_config,
      handler
    )
  )
}
