#lang racket/base

(require (for-syntax syntax/parse
                     racket/base)
         ffi/unsafe
         racket/bool
         racket/function
         (rename-in racket/contract [-> -->])
         "mqtt-client-ffi.rkt")



(provide mqtt/with-client
         mqtt/with-connection
         mqtt/with-timeout
         mqtt/with-qos
         mqtt/publish
         mqtt/subscribe
         mqtt/with-message-recv
         mqtt/will
         mqtt/qos?
         mqtt/mqtt-version?
         mqtt/will?)


(define current-client
  (make-parameter
   #f
   (lambda (x)
     (if (cpointer? x)
         x
         (raise-mqtt-error
          'mqtt/with-client
          (format "client must be cpointer which includes #f but was ~a" x))))))

(define current-connect-options
  (make-parameter
   #f
   (lambda (x)
     (if (or (MQTTClient_connectOptions? x) (false? x))
         x
         (raise-mqtt-error
          'mqtt/with-connection
          (format "connect-options must be mqtt/connect-options or #f but was ~a" x))))))

(define current-qos
  (make-parameter
   'qos-2
   (lambda (x)
     (if (mqtt/qos? x)
         x
         (raise-mqtt-error
          'mqtt/with-qos
          (format "qos must be either qos-0, qos-1, or qos-2 but was ~a" x))))))

(define current-timeout
  (make-parameter
   15000
   (lambda (x)
     (if (exact-positive-integer? x)
         x
         (raise-mqtt-error
          'with-timeout
          (format "timeout must be exact, positive integer but was ~a" x))))))

(define/contract (validate-default-message th)
  (--> (--> any/c) (--> any/c))
  th)

(define current-default-message
  (make-parameter
   (thunk #f)
   validate-default-message))

(define (client-destroy)
  (when (current-client)
    (MQTTClient_destroy (current-client)))
  (current-client #f))

(define (client-disconnect)
  (when (and (current-client) (current-connect-options))
    (MQTTClient_disconnect
     (current-client)
     (MQTTClient_connectOptions-keepAliveInterval (current-connect-options))))
  (current-connect-options #f))


(define (mqtt/will topic message #:retained [retained #f])
  (create-MQTTClient_willOptions topic message retained (current-qos)))

(define-syntax (mqtt/with-client stx)
  (syntax-parse stx
    [(_ (server-uri client-id) body ...)
     #'(parameterize ([current-client (MQTTClient_create server-uri client-id 'persistence-none #f)])
         (with-handlers ([any/c (lambda (e) (client-destroy) (raise e))])
           (begin0
             (begin
               (void)
               body ...)
             (client-destroy))))]
    [(_ (server-uri client-id persist-dir) body ...)
     #'(let ([persist-dir (if (path? persist-dir)
                              (path->string persist-dir)
                              persist-dir)])
         (parameterize ([current-client (MQTTClient_create server-uri client-id 'persistence-default persist-dir)])
           (with-handlers ([any/c (lambda (e) (client-destroy) (raise e))])
             (begin0
               (begin
                 (void)
                 body ...)
               (client-destroy)))))]))

(define-syntax (mqtt/with-connection stx)
  (syntax-parse stx
    [(_ (args ...) body ...)
     #'(if (current-client)
           (parameterize ([current-connect-options (create-MQTTClient_connectOptions args ...)])
             (with-handlers ([any/c (lambda (e) (client-disconnect) (raise e))])
               (begin0
                 (begin
                   (MQTTClient_connect (current-client) (current-connect-options))
                   body ...)
                 (client-disconnect))))
           (raise-mqtt-error
            'mqtt/with-connection
            "mqtt/with-connection must appear in the body of an mqtt/with-client form"))]))

(define (validate-qos qos)
  (cond
    [(exact-nonnegative-integer? qos) (validate-qos (string->symbol (format "qos-~a" qos)))]
    [(mqtt/qos? qos)                       qos]
    [#t                               (error (format "invalid QOS: ~a" qos))]))

(define-syntax (mqtt/with-qos stx)
  (syntax-parse stx
    [(_ (qos:expr) body ...)
     #'(parameterize ([current-qos (validate-qos qos)])
         (void)
         body ...)]))

(define-syntax (mqtt/with-timeout stx)
  (syntax-parse stx
    [(_ (timeout) body ...)
     #'(parameterize ([current-timeout timeout])
         (void)
         body ...)]))




(define (mqtt/publish topic payload #:retained [retained #f])

  (define msg
    (create-MQTTClient_message
     payload
     #:qos      (current-qos)
     #:retained retained))

  (define dt
    (MQTTClient_publishMessage (current-client) topic msg))

  (MQTTClient_waitForCompletion (current-client) dt (current-timeout)))


(define (mqtt/subscribe topic)
  (MQTTClient_subscribe (current-client) topic (current-qos)))


(define-syntax (mqtt/with-default-message stx)
  (syntax-parse stx
    [(_ (value) body ...)
     #'(parameterize ([current-default-message (thunk value)])
         (void)
         body ...)]))


(define-syntax (mqtt/with-message-recv stx)
  (syntax-parse stx
    [(_ (topic payload) body ...)
     (let ([x (gensym)])
       #`(let-values ([(#,x topic) (MQTTClient_receive (current-client) (current-timeout))])
           (if #,x
               (let ([payload (MQTTClient_message-payload #,x)])
                 (void)
                 body ...)
               ((current-default-message)))))]))
         
         
(define mqtt/qos?
  (or/c 'qos-0 'qos-1 'qos-2))

(define mqtt/mqtt-version?
  (or/c 'mqtt-version-default 'mqtt-version-3-1 'mqtt-version-3-1-1 'mqtt-version-5))

(define mqtt/will? MQTTClient_willOptions?)