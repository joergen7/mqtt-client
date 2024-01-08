#lang racket/base

(require mqtt-client)

(mqtt/with-client ("localhost" "client1")

  (mqtt/with-qos ('qos-1)
                 
    (mqtt/with-connection (#:keep-alive-interval 20
                           #:clean-session       #t)

                           
      (mqtt/subscribe "some-topic")

      (mqtt/publish "some-topic" "Hello world")

      (mqtt/with-message-recv (topic payload)
        (displayln topic)
        (displayln payload)))))
  
                          
  