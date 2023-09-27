#lang racket/base

(require (for-syntax (only-in syntax/parse
                              syntax-parse)
                     (only-in racket/base
                              syntax))
         (only-in ffi/unsafe
                  cpointer?)

         (only-in racket/contract
                  any/c)

         (only-in racket/bool
                  false?)

         (only-in "mqtt-client-ffi.rkt"
                  mqtt/error
                  mqtt/create
                  mqtt/destroy
                  mqtt/connect
                  mqtt/disconnect
                  mqtt/connect-options
                  mqtt/connect-options?
                  mqtt/connect-options-keep-alive-interval
                  mqtt/message
                  mqtt/publish-message
                  mqtt/wait-for-completion))



(provide mqtt/with-client
         mqtt/with-connection
         mqtt/with-timeout
         mqtt/with-qos
         mqtt/publish)


(define current-client
  (make-parameter
   #f
   (lambda (x)
     (if (cpointer? x)
         x
         (mqtt/error
          'mqtt/with-client
          (format "client must be cpointer which includes #f but was ~a" x))))))

(define current-connect-options
  (make-parameter
   #f
   (lambda (x)
     (if (or (mqtt/connect-options? x) (false? x))
         x
         (mqtt/error
          'mqtt/with-connection
          (format "connect-options must be mqtt/connect-options or #f but was ~a" x))))))

(define current-qos
  (make-parameter
   'qos-0
   (lambda (x)
     (if (member x '(qos-0 qos-1 qos-2))
         x
         (mqtt/error
          'mqtt/with-qos
          (format "qos must be either qos-0, qos-1, or qos-2 but was ~a" x))))))

(define current-timeout
  (make-parameter
   15000
   (lambda (x)
     (if (exact-positive-integer? x)
         x
         (mqtt/error
          'with-timeout
          (format "timeout must be exact, positive integer but was ~a" x))))))

(define (client-destroy)
  (when (current-client)
    (mqtt/destroy (current-client)))
  (current-client #f))

(define (client-disconnect)
  (when (and (current-client) (current-connect-options))
    (mqtt/disconnect
     (current-client)
     (mqtt/connect-options-keep-alive-interval (current-connect-options))))
  (current-connect-options #f))

(define-syntax (mqtt/with-client stx)
  (syntax-parse stx
    [(_ (server-uri client-id) body ...)
     #'(parameterize ([current-client (mqtt/create server-uri client-id 'persistence-none #f)])
         (with-handlers ([any/c (lambda (e) (client-destroy) (raise e))])
           body ...
           (client-destroy)))]
    [(_ (server-uri client-id persist-dir) body ...)
     #'(let ([persist-dir (if (path? persist-dir) (path->string persist-dir) persist-dir)])
         (parameterize ([current-client (mqtt/create server-uri client-id 'persistence-default persist-dir)])
           (with-handlers ([any/c (lambda (e) (client-destroy) (raise e))])
             body ...
             (client-destroy))))]))

(define-syntax (mqtt/with-connection stx)
  (syntax-parse stx
    [(_ (args ...) body ...)
     #'(if (current-client)
           (parameterize ([current-connect-options (mqtt/connect-options args ...)])
             (with-handlers ([any/c (lambda (e) (client-disconnect) (raise e))])
               (mqtt/connect (current-client) (current-connect-options))
               body ...
               (client-disconnect)))
           (mqtt/error
            'mqtt/with-connection
            "mqtt/with-connection must appear in the body of an mqtt/with-client form"))]))

(define-syntax (mqtt/with-qos stx)
  (syntax-parse stx
    [(_ (qos) body ...)
     #'(parameterize ([current-qos 'qos])
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
    (mqtt/message
     payload
     #:qos      (current-qos)
     #:retained retained))

  (define dt
    (mqtt/publish-message (current-client) topic msg))

  (mqtt/wait-for-completion (current-client) dt (current-timeout)))

