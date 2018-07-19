(ns repl-tooling.repl-client.protocols)

(defprotocol Repl
  (treat-data [_ data])
  (send-command [_ command]))
