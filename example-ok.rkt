#lang racket/base

(require mqtt-client)


(define server-uri "tcp://localhost:1883")
(define client-id  "ExampleClientPub")
(define topic      "MQTT Examples")
(define payload    #"Hello World!")
(define timeout    10000)


;; create MQTT client context
(define client
  (MQTTClient_create server-uri client-id 'persistence-none #f))

;; define connection options
(define conn-opts
  (create-MQTTClient_connectOptions #:keep-alive-interval 20 #:clean-session #t))

;; connect to MQTT provider using connection options
(MQTTClient_connect client conn-opts)

;; subscribe to topic
(MQTTClient_subscribe client topic 'qos-2)

;; define message to publish
(define pub-msg
  (create-MQTTClient_message payload #:qos 'qos-1 #:retained #f))

;; publish message
(define dt
  (MQTTClient_publishMessage client topic pub-msg))

;; wait for completion
(MQTTClient_waitForCompletion client dt timeout)

;; attempt to receive message
(define-values (recv-msg recv-topic)
  (MQTTClient_receive client timeout))

;; print the received message if possible
(when recv-msg
  (displayln recv-topic)
  (displayln (MQTTClient_message-payload recv-msg))
  (displayln (MQTTClient_message-qos recv-msg)))

;; drop connection
(MQTTClient_disconnect client timeout)

;; drop client context
(MQTTClient_destroy client)