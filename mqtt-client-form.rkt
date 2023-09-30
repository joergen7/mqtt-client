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
         mqtt/with-publish-qos
         mqtt/publish
         mqtt/subscribe
         mqtt/with-message-recv)


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
     (if (or (connect-options? x) (false? x))
         x
         (raise-mqtt-error
          'mqtt/with-connection
          (format "connect-options must be mqtt/connect-options or #f but was ~a" x))))))

(define current-publish-qos
  (make-parameter
   'qos-0
   (lambda (x)
     (if (member x '(qos-0 qos-1 qos-2))
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
     (connect-options-keepAliveInterval (current-connect-options))))
  (current-connect-options #f))

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
           (parameterize ([current-connect-options (create-connect-options args ...)])
             (with-handlers ([any/c (lambda (e) (client-disconnect) (raise e))])
               (begin0
                 (begin
                   (MQTTClient_connect (current-client) (current-connect-options))
                   body ...)
                 (client-disconnect))))
           (raise-mqtt-error
            'mqtt/with-connection
            "mqtt/with-connection must appear in the body of an mqtt/with-client form"))]))

(define-syntax (mqtt/with-publish-qos stx)
  (syntax-parse stx
    [(_ (qos) body ...)
     #'(parameterize ([current-publish-qos 'qos])
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
    (create-message
     payload
     #:qos      (current-publish-qos)
     #:retained retained))

  (define dt
    (MQTTClient_publishMessage (current-client) topic msg))

  (MQTTClient_waitForCompletion (current-client) dt (current-timeout)))


(define (mqtt/subscribe topic #:qos [qos 'qos-2])
  (MQTTClient_subscribe (current-client) topic qos))


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
               (let ([payload (message-payload #,x)])
                 (void)
                 body ...)
               ((current-default-message)))))]))
         
         
  
  