#lang scribble/manual
@require[@for-label[
 mqtt-client
 racket/base
 ]]

@title{mqtt-client}
@author{Jörgen Brandt}

@defmodule[mqtt-client]

The @racketmodname[mqtt-client] module is a Racket MQTT client implementation based on @link["https://github.com/eclipse/paho.mqtt.c"]{libpaho-mqtt3}.
It allows the user to set up an MQTT client, create a connection to MQTT message broker, subscribe to topics, and to send
or receive messages. The module uses a C-based, dynamically linked library that needs to reside in the operating system.

@section{Example}

Below, we list a simple example for the way, this module might be used. To send and receive messages we set up different
contexts, two of which are mandatory: a client context and, nested in it, a connection context. Optionally, we can change
some parameters using contexts. Here, for example, we change the quality of service (QOS) when publishing to @racket[qos-1].

First, we set up a client using @racket[mqtt/with-client]. Herein, we provide the message broker's URL, @racket["localhost"],
and a client id, @racket["client1"], for our application.

Next, we set up a connection to an MQTT message broker using @link[]{mqtt/with-connection}. All connection parameters have
reasonable default values but, here, we set the keep-alive interval to @racket[20] seconds and request a clean session.

Lastly, we set the QOS for publishing to @racket[qos-1] using @racket[mqtt/with-publish-qos]. Note, the QOS for any message an
MQTT client receives is determined by the publisher and, thus, needs no parameterization on the client's part.

Within these three nested contexts, setting up a client, a connection, and default parameters (here for the QOS), we can
subscribe to topics, send messages, and wait for messages to be received. Here, we use @racket[mqtt/subscribe] to subscribe to
the topic @racket["some-topic"]. We use @racket[mqtt/publish] to publish the message @racket["Hello World"]. Finally, we
receive a message using @racket[mqtt/with-message-recv].

The form @racket[mqtt/with-message-recv] creates its own context, in which two named variables are defined designating the
topic name on which the message was received and the message payload. Here, we call these variables @racket[topic] and
@racket[payload] and display them.



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

@defproc[(mqtt/publish [topic string?]
                       [payload string?]
                       [#:retained retained boolean? #f])
         void?]|{
}|
