#lang racket/base

(require mqtt-client)


(define server-uri "tcp://localhost:1883")
(define client-id  "ExampleClientPub")
(define topic      "MQTT Examples")
(define payload    "Hello World!")
(define timeout    10000)


;; create MQTT client context
(define client
  (mqtt/create-inmem server-uri client-id))

;; define connection options
(define conn-opts
  (mqtt/connect-options #:keep-alive-interval 20 #:clean-session #t))

;; connect to MQTT provider using connection options
(mqtt/connect client conn-opts)

;; subscribe to topic
(mqtt/subscribe client topic)

;; define message to publish
(define pub-msg
  (mqtt/message payload #:qos 'qos-1 #:retained #f))

;; publish message
(define dt
  (mqtt/publish-message client topic pub-msg))

;; wait for completion
(mqtt/wait-for-completion client dt timeout)

;; attempt to receive message
(define-values (recv-msg recv-topic)
  (mqtt/receive client timeout))

;; print the received message if possible
(when recv-msg
  (displayln recv-topic)
  (displayln (mqtt/message-payload recv-msg))
  (displayln (mqtt/message-qos recv-msg)))

;; drop connection
(mqtt/disconnect client timeout)

;; drop client context
(mqtt/destroy client)