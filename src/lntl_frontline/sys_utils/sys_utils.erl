-module(sys_utils).
-author("Daniel Jeremiah <danielshunom2@gmail.com>").
-export([start_osmon/0, stop_osmon/0, get_mem/0, get_load/0]).

-spec start_osmon() -> {ok, pid()} | {error, any()}.
start_osmon() -> 
  case application:start(sasl) of
    ok -> application:start(os_mon);
    error -> error
  end.

-spec stop_osmon() -> {ok, any()} | {error, any()}.
stop_osmon() ->
  case application:stop(os_mon) of
    ok -> application:stop(sasl);
    error -> error
  end.

% get memory allocation and consumption on machine
-spec get_mem() -> {int, int}.
get_mem() ->
  {All, Used, _} = memsup:get_memory_data(),
  {All, Used}.

% get cpu load with val between 0 and 256 inclusive.
-spec get_load() -> integer() | {error, any()}.
get_load() ->
  cpu_sup:avg1().
