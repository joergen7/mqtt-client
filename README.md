# mqtt-client

Racket MQTT client implementation based on paho.mqtt.c

## Introduction

This package provides an MQTT client implementation enabling Racket applications to exchange messages with an MQTT broker. This client implementation is based on [paho.mqtt.c](https://github.com/eclipse/paho.mqtt.c) using Racket's foreign function interface `ffi/unsafe` to wrap the synchronous MQTT library calls.

## Example

```racket
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
```

## Related Work

- [mosquitto-ffi](https://pkgs.racket-lang.org/package/mosquitto-ffi) An FFI binding of libmosquitto for racket

## Prerequisites

### paho.mqtt.c Library

As this MQTT client wraps a library, this library must be present in the environment Racket is running in. E.g., on Ubuntu paho.mqtt.c can be installed by running:

    sudo apt-get install -y libpaho-mqtt1.3
	
### MQTT Broker

To exchange messages an MQTT broker must be available. [Eclipse Mosquitto](https://mosquitto.org/) is a popular MQTT broker. To install Eclipse Mosquitto on Ubuntu run:

    sudo apt-get install -y mosquitto
   
Verify the broker service is running be entering:

    systemctl status mosquitto
	
## Installation

### from Racket Package Index Catalog

To install this package from the internet, run

    raco pkg install mqtt-client


### from Source

To install this package from this source repository in the `mqtt-client` directory run

    raco pkg install
	
## Removal

To remove this package run

    raco pkg remove mqtt-client
	


