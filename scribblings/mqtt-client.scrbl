#lang scribble/manual
@require[@for-label[
 mqtt-client
 racket/base
 racket/contract
 racket/bool
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

Lastly, we set the QOS for publishing and subscribing to @racket[qos-1] using @racket[mqtt/with-publish-qos].

Within these three nested contexts, setting up a client, a connection, and  QOS, we can
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

    (mqtt/with-qos ('qos-1)
                           
      (mqtt/subscribe "some-topic")

      (mqtt/publish "some-topic" "Hello world")

      (mqtt/with-message-recv (topic payload)
        (displayln topic)
        (displayln payload)))))
}|


@section{MQTT API}

The MQTT API can be used to exchange messages via MQTT in a functional style
where the state of the application is managed in the form of scopes and construction and
destruction of stateful objects is implicit.

@subsection{Predicates}

@defproc[#:kind "predicate" (mqtt/qos? [x any/c]) boolean?]{
 Predicate identifying a quality of service (QOS). Either one of three symbols: @racket['qos-0],
 @racket['qos-1], or @racket['qos-2].
}

@defproc[#:kind "predicate" (mqtt/mqtt-version? [x any/c]) boolean?]{
 Predicate identifying an MQTT version. Either one of four symbols: @racket['mqtt-version-default],
 @racket['mqtt-version-3-1], @racket['mqtt-version-3-1-1], or @racket['mqtt-version-5].
}

@defproc[#:kind "predicate" (mqtt/will? [x any/c]) boolean?]{
 Predicate identifying a will object created with @racket[mqtt/will].
}

@subsection{Connections}

This module manages MQTT connections using context forms. A context form is a form consisting of
a head in which parameters are set and a body that is enriched by a context that adheres to the
aforementioned parameters.

Two context forms are necessary to create a connection to an MQTT message broker:
@racket[mqtt/with-client] and @racket[mqtt/with-connection]. Herein, @racket[mqtt/with-client]
prepares an MQTT client by fixing the message broker URI and the client id.
@racket[mqtt/with-connection], in turn, configures a concrete connection allowing the user to
set parameters like the keep-alive interval, or the will.

In addition, there are two optional context forms: @racket[mqtt/with-qos] to set the QOS and
@racket[mqtt/with-timeout] to set the timeout for publishing and receiving.

@defform*[[(mqtt/with-client (server-uri client-id) body ...)
           (mqtt/with-client (server-uri client-id persist-dir) body ...)]
         #:contracts ([server-uri  string?]
                      [client-id   string?]
                      [persist-dir path-string?]
                      [body        any/c])]{
 Context form, initializing an MQTT client communicating to a message broker identified by the
 @racket[server-uri]. The @racket[client-id] is a unique label the client gives itself. The body
 can be any kind and any number of Racket expressions including an @racket[mqtt/with-connection]
 context form.

 If @racket[persist-dir] is given, then the client state is persisted in that directory instead
 of in-memory.
}

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
                      [will                  (or/c false? mqtt/will?)]
                      [username              (or/c false? string?)]
                      [password              (or/c false? string?)]
                      [connect-timeout       exact-nonnegative-integer?]
                      [retry-interval        exact-nonnegative-integer?]
                      [mqtt-version          mqtt-version?]
                      [max-inflight-messages exact-integer?]
                      [clean-start           boolean?]
                      [http-proxy            (or/c false? string?)]
                      [https-proxy           (or/c false? string?)]
                      [body                  any/c])]{
 Context form to set up an MQTT connection. Must be used in the body of a client context
 created using @racket[mqtt/with-client]. Setting the will requires creating a will object using
 @racket[mqtt/will].
}

@defproc[#:kind "constructor"
         (mqtt/will
          [topic   string?]
          [message string?]
          [#:retained retained boolean? #f])
         mqtt/will?]{
 Constructor to create a will object to be used in the head of a @racket[mqtt/with-connection]
 context form. The QOS is set to the value defined with @racket[mqtt/with-qos].
}

@defform[(mqtt/with-timeout (timeout) body ...)
         #:contracts ([timeout exact-positive-integer?]
                      [body    any/c])]{
 Context form for setting the timeout for publishing and receiving operations in milliseconds.
 Specifically, the expressions @racket[mqtt/publish] and @racket[mqtt/with-message-recv] use
 this timeout value. If the context form is omitted, the timeout defaults
 to @racket[15000] milliseconds.
}

@defform[(mqtt/with-qos (qos) body ...)
         #:contracts ([qos  (or/c qos? 0 1 2)]
                      [body any/c])]{
 Context form for setting the QOS for publishing. If the @racket[mqtt/with-qos] form is
 omitted, the QOS defaults to @racket['qos-2].
}



@subsection{Publishing}

@defproc[(mqtt/publish [topic string?]
                       [payload string?]
                       [#:retained retained boolean? #f])
         void?]{
 Publish a message @racket[payload] to the given @racket[topic]. Optionally, the message can be
 flagged as retained using the keyword argument @racket[#:retained] which defaults to @racket[#f].
 The QOS is set to the value defined with @racket[mqtt/with-qos].
}

@subsection{Subscriptions}

@defproc[(mqtt/subscribe [topic string?]) void?]{
 Subscribe the client to the given @racket[topic]. The QOS is set to the value defined with
 @racket[mqtt/with-qos].
}

@defform[(mqtt/with-message-recv (topic payload) body ...)
         #:grammar ([topic
                     (code:line id)]
                    [payload
                     (code:line id)])
         #:contracts ([body    any/c])]{
 Context form for receiving a message from the message broker. In the body of the form the variables
 identified by @racket[topic] and @racket[payload] are defined both having string values.
}

