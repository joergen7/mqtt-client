#lang info
(define collection "mqtt-client")
(define deps '("base"
               ("libpaho-mqtt3-x86_64-linux-natipkg" #:platform "x86_64-linux-natipkg")))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/mqtt-client.scrbl" ())))
(define pkg-desc "Racket MQTT client implementation based on paho.mqtt.c")
(define version "0.1")
(define pkg-authors '("Jörgen Brandt"))
(define license 'Apache-2.0)
