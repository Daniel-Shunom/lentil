import dot_env
import dot_env/env
import gleam/bit_array
import gleam/crypto.{Sha512}
import gleam/int
import gleam/option.{Some}
import gleam/result
import gleam/time/calendar.{utc_offset}
import gleam/time/duration.{to_seconds_and_nanoseconds}
import gleam/time/timestamp.{from_unix_seconds_and_nanoseconds}
import global/gtypes.{type LentilTimeStamp}
import pog
import prng/random
import prng/seed

pub fn initialize_env() -> Nil {
  dot_env.new()
  |> dot_env.set_path(".env")
  |> dot_env.set_debug(True)
  |> dot_env.load()
}

pub fn get_env(val_key: String) -> String {
  let assert Ok(secret) = env.get_string(val_key)
  secret
}

// NOTE -> this gets a universal epoch timestamp
pub fn get_timestamp() -> LentilTimeStamp {
  let #(seconds, nano_seconds) = to_seconds_and_nanoseconds(utc_offset)
  gtypes.LentilTimeStamp(time: from_unix_seconds_and_nanoseconds(
    seconds,
    nano_seconds,
  ))
}

pub fn id_generator(prefix: String) -> String {
  let str = random.fixed_size_string(32)
  let num = random.int(0, 100_000)
  let secure_prefix =
    random.int(0, 13)
    |> random.random_sample()
    |> seed.new()
    |> random.sample(num, _)
    |> int.to_string()

  let secure_id =
    random.int(0, 9)
    |> random.random_sample()
    |> seed.new()
    |> random.sample(str, _)

  prefix <> secure_prefix <> secure_id
}

pub fn hasher(str: String) -> String {
  bit_array.from_string(str)
  |> crypto.hash(Sha512, _)
  |> bit_array.base64_encode(True)
}

pub fn connect_lentildb() -> pog.Connection {
  pog.connect(lentildb_config())
}

fn lentildb_config() -> pog.Config {
  initialize_env()
  let pgport =
    int.parse(get_env("PGPORT"))
    |> result.unwrap(5432)
  pog.Config(
    host: get_env("PGHOST"),
    port: pgport,
    database: get_env("PGDATABASE"),
    user: get_env("PGUSER"),
    password: Some(get_env("PGPASSWORD")),
    ssl: pog.SslDisabled,
    connection_parameters: [],
    pool_size: 10,
    queue_target: 50,
    queue_interval: 1000,
    idle_interval: 1000,
    trace: False,
    ip_version: pog.Ipv4,
    rows_as_map: False,
    default_timeout: 5000,
  )
}
