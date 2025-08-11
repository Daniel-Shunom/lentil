import configs/configs.{type DbConfig, DbConfig}

pub fn dbconfig_new() -> DbConfig {
  DbConfig(5432, 3000, 10, "", "")
}

pub fn dbconfig_set_port(config: DbConfig, port: Int) -> DbConfig {
  DbConfig(..config, port: port)
}

pub fn dbconfig_set_host(config: DbConfig, host: Int) -> DbConfig {
  DbConfig(..config, host: host)
}

pub fn dbconfig_set_poolsize(config: DbConfig, poolsize) -> DbConfig {
  DbConfig(..config, poolsize: poolsize)
}

pub fn dbconfig_set_username(config: DbConfig, username: String) -> DbConfig {
  DbConfig(..config, username: username)
}

pub fn dbconfig_set_password(config: DbConfig, password: String) -> DbConfig {
  DbConfig(..config, password: password)
}
