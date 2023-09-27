#lang scribble/manual
@require[@for-label[mqtt-client
                    racket/base]]

@title{mqtt-client}
@author{Jörgen Brandt}

@defmodule[mqtt-client]

This module is a Racket MQTT client implementation based on paho.mqtt.c.

@defproc[(mqtt/create-inmem
          [server-uri string?]
          [client-id string?])
         cpointer?]
{
Creates an MQTT client context ready for connection. The procedure creates the MQTT
client context in memory, i.e., with no persistence.
}

@defproc[(mqtt/create-fs
          [server-uri string?]
          [client-id string?]
          [#:persist-dir persist-dir (or/c false? path-string?) #f])
         cpointer?]
{
Creates an MQTT client context ready for connection. The procedure creates the MQTT
client context with filesystem-based persistence. Setting persist-dir to #f creates
the persistence in the current directory. Otherwise, persist-dir is used as the
directory to persist session information.
}
