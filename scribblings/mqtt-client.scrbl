#lang scribble/manual
@require[@for-label[mqtt-client
                    racket/base
                    racket/contract
                    racket/bool]]

@title{mqtt-client}
@author{Jörgen Brandt}

@defmodule[mqtt-client]

This module is a Racket MQTT client implementation based on @link["https://github.com/eclipse/paho.mqtt.c"]{libpaho-mqtt3}.
It allows the user to set up an MQTT client, create a connection to MQTT message broker, subscribe to topics, and to send
or receive messages. The module uses a C-based MQTT library that needs to reside in the operating system, the Racket
instance runs in.

@section{Example}

Below, we list a simple example for the way, this module might be used. In the example, we set up a client with
@link["MQTT_API.html#(form._((lib._mqtt-client%2Fmain..rkt)._mqtt%2Fwith-client))"]{mqtt/with-client}

@codeblock|{
#lang racket/base

(require mqtt-client)

(mqtt/with-client ("localhost" "client1")

  (mqtt/with-connection (#:keep-alive-interval 20
                         #:clean-session       #t)

    (mqtt/with-publish-qos (qos-1)
                           
      (mqtt/subscribe "some-topic")

      (mqtt/publish "some-topic" "Hello world")

      (mqtt/with-message-recv (topic payload)
        (displayln topic)
        (displayln payload)))))
}|


@section{MQTT API}

@defform[(mqtt/with-client (server-uri client-id) body ...)
         #:contracts ([server-uri string?]
                      [client-id  string?]
                      [body       any/c])]|{
some text
}|

@defform[(mqtt/with-connection (binding ...) body ...)
         #:grammar ([binding
                     (code:line #:keep-alive-interval   keep-alive-interval)
                     (code:line #:clean-session         clean-session)
                     (code:line #:reliable              reliable)
                     (code:line #:will                  will)
                     (code:line #:username              username)
                     (code:line #:password              password)
                     (code:line #:connect-timeout       connect-timeout)
                     (code:line #:retry-interval        retry-interval)
                     (code:line #:mqtt-version          mqtt-version)
                     (code:line #:max-inflight-messages max-inflight-messages)
                     (code:line #:clean-start           clean-start)
                     (code:line #:http-proxy            http-proxy)
                     (code:line #:https-proxy           https-proxy)])
         #:contracts ([keep-alive-interval   exact-nonnegative-integer?]
                      [clean-session         boolean?]
                      [reliable              boolean?]
                      [will                  (or/c false? MQTTClient_willOptions?)]
                      [username              (or/c false? string?)]
                      [password              (or/c false? string?)]
                      [connect-timeout       exact-nonnegative-integer?]
                      [retry-interval        exact-nonnegative-integer?]
                      [mqtt-version          (or/c 'mqtt-version-default 'mqtt-version-3-1 'mqtt-version-3-1-1 'mqtt-version-5)]
                      [max-inflight-messages exact-integer?]
                      [clean-start           boolean?]
                      [http-proxy            (or/c false? string?)]
                      [https-proxy           (or/c false? string?)]
                      [body                  any/c])]|{
some other text
}|



@defproc[#:kind "constructor"
         (mqtt/will
          [topic   string?]
          [message string?]
          [#:retained retained boolean? #f]
          [#:qos qos (or/c 'qos-0 'qos-1 'qos-2) 'qos-0])
         MQTTClient_willOptions?]|{

}|