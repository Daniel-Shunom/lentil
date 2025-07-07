import lntl_frontline/msg_types as mt

pub type LoggerMessage(msg) {
  LogClientEvent(mt.ClientRouterMessage(msg))
  LogServerEvent(mt.ServerRouterMessage(msg))
  FlushLogs
  SetLogLevel(String)
  // Optional: dynamic verbosity
}
