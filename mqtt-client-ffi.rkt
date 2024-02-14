#lang racket/base


;;============================================================
;; Require and Provide
;;============================================================

(require
  (for-syntax syntax/parse
              racket/base)
  ffi/unsafe  
  ffi/unsafe/define
  ffi/unsafe/alloc

  (only-in racket/match
           define/match))



  
(provide
 (struct-out exn:fail:mqtt)             raise-mqtt-error
 (struct-out MQTTClient_willOptions)    create-MQTTClient_willOptions
 (struct-out MQTTClient_connectOptions) create-MQTTClient_connectOptions
 (struct-out MQTTClient_message)        create-MQTTClient_message
 MQTTClient_destroy
 MQTTClient_create
 MQTTClient_connect
 MQTTClient_disconnect
 MQTTClient_publishMessage
 MQTTClient_waitForCompletion
 MQTTClient_subscribe
 MQTTClient_yield
 MQTTClient_receive)


;;============================================================
;; Error Handling
;;============================================================

(struct exn:fail:mqtt exn:fail ()
  #:transparent)

(define-syntax (raise-mqtt-error stx)
  (syntax-parse stx
    [(_ who reason)
     #'(raise
        (exn:fail:mqtt
         (format "~a: ~a" who reason)
         (current-continuation-marks)))]))

(define/match (check v who)
  [(0   _)   (void)]
  [(-1  who) (raise-mqtt-error who "failure")]
  [(-2  who) (raise-mqtt-error who "persistence error")]
  [(-3  who) (raise-mqtt-error who "disconnected")]
  [(-4  who) (raise-mqtt-error who "maximum messages in flight")]
  [(-5  who) (raise-mqtt-error who "bad UTF-8 string")]
  [(-6  who) (raise-mqtt-error who "null parameter")]
  [(-7  who) (raise-mqtt-error who "topicname truncated")]
  [(-8  who) (raise-mqtt-error who "bad-structure")]
  [(-9  who) (raise-mqtt-error who "bad QOS")]
  [(-10 who) (raise-mqtt-error who "SSL not supported")]
  [(-11 who) (raise-mqtt-error who "bad MQTT version")]
  [(-14 who) (raise-mqtt-error who "bad protocol")]
  [(-15 who) (raise-mqtt-error who "bad MQTT option")]
  [(-16 who) (raise-mqtt-error who "wrong MQTT version")]
  [(-17 who) (raise-mqtt-error who "0-length will topic")]
  [((? exact-integer? n) who) (raise-mqtt-error who (number->string n))])



;;============================================================
;; Data Types
;;============================================================

(define _MQTTClient-pointer                 (_cpointer 'mqtt-client))
(define _MQTTClient_deliveryToken-pointer   (_cpointer 'delivery-token))
(define _MQTTClient_SSLOptions-pointer/null (_cpointer/null 'ssl-options))
(define _MQTTClient_nameValue-pointer/null  (_cpointer/null 'name-value))
(define _MQTTProperty-pointer/null          (_cpointer/null 'mqtt-property))

(define _persistence-type
  (_enum '(persistence-default
           persistence-none
           persistence-user)))

(define _mqtt-version
  (_enum '(mqtt-version-default = 0
           mqtt-version-3-1     = 3
           mqtt-version-3-1-1   = 4
           mqtt-version-5       = 5)))

(define _qos
  (_enum '(qos-0
           qos-1
           qos-2)))

(define _struct_id
  (_array/list _byte 4))


;; payload
;;------------------------------------------------------------

(define-cstruct _payload
  ([len  _int]
   [data (_cpointer/null 'data)]))
  


;; will
;;------------------------------------------------------------

(define-cstruct _MQTTClient_willOptions
  ([struct_id      _struct_id]
   [struct_version _int]
   [topicName      _string/utf-8]
   [message        _bytes/nul-terminated]
   [retained       _bool]
   [qos            _qos]
   [payload        _payload]))

(define (create-MQTTClient_willOptions
         topic
         message
         #:retained [retained #f]
         #:qos      [qos 'qos-2])
  (make-MQTTClient_willOptions
   (map char->integer '(#\M #\Q #\T #\W))
   1
   topic
   message
   retained
   qos
   (make-payload 0 #f)))


;; returned
;;------------------------------------------------------------

(define-cstruct _returned
  ([serverURI      _string/utf-8]
   [MQTTVersion    _int]
   [sessionPresent _int]))

;; binarypwd
;;------------------------------------------------------------

(define-cstruct _binarypwd
  ([len  _int]
   [data (_cpointer/null 'data)]))
  
;; connect-options
;;------------------------------------------------------------

(define-cstruct _MQTTClient_connectOptions
  ([struct_id           _struct_id]
   [struct_version      _int]
   [keepAliveInterval   _int]
   [cleansession        _bool]
   [reliable            _bool]
   [will                _MQTTClient_willOptions-pointer/null]
   [username            _string/utf-8]
   [password            _string/utf-8]
   [connectTimeout      _int]
   [retryInterval       _int]
   [ssl                 _MQTTClient_SSLOptions-pointer/null]
   [serverURIcount      _int]
   [serverURIs          (_cpointer/null 'server-uris)]
   [MQTTVersion         _mqtt-version]
   [returned            _returned]
   [binarypwd           _binarypwd]
   [maxInflightMessages _int]
   [cleanstart          _bool]
   [httpHeaders         _MQTTClient_nameValue-pointer/null]
   [httpProxy           _string/utf-8]
   [httpsProxy          _string/utf-8]))

(define (create-MQTTClient_connectOptions
         #:keep-alive-interval   [keep-alive-interval   60] ; time [s]
         #:clean-session         [clean-session         #t]
         #:reliable              [reliable              #t]
         #:will                  [will                  #f]
         #:username              [username              #f]
         #:password              [password              #f]
         #:connect-timeout       [connect-timeout       30] ; time [s]
         #:retry-interval        [retry-interval         0] ; time [s] 0 means no retries
         #:mqtt-version          [mqtt-version          'mqtt-version-default]
         #:max-inflight-messages [max-inflight-messages -1]
         #:clean-start           [clean-start           #f]
         #:http-proxy            [http-proxy            #f]
         #:https-proxy           [https-proxy           #f])
  (make-MQTTClient_connectOptions
   (map char->integer '(#\M #\Q #\T #\C)) ; eyecatcher is fixed
   8                                      ; struct-version is fixed
   keep-alive-interval
   clean-session
   reliable
   will
   username
   password
   connect-timeout
   retry-interval
   #f                                     ; ssl
   0                                      ; server-uri-count
   #f                                     ; server-uris
   mqtt-version
   (make-returned #f 0 0)                 ; server sets returned
   (make-binarypwd 0 #f)                  ; binarypwd
   max-inflight-messages
   clean-start
   #f                                     ; http-headers
   http-proxy
   https-proxy))


;; mqtt-properties
;;------------------------------------------------------------

(define-cstruct _MQTTProperties
  ([count     _int]
   [max_count _int]
   [length    _int]
   [array     _MQTTProperty-pointer/null]))

(define (create-MQTTProperties
         #:count     [count      0]
         #:max-count [max-count  0]
         #:length    [length     0]
         #:array     [array     #f])
  (make-MQTTProperties count max-count length array))

;; message
;;------------------------------------------------------------

(define-cstruct _MQTTClient_message
  ([struct_id      _struct_id]
   [struct_version _int]
   [payloadlen     _int]
   [payload        _bytes]
   [qos            _qos]
   [retained       _bool]
   [dup            _bool]
   [msgid          _int]
   [properties     _MQTTProperties]))

(define (create-MQTTClient_message
         payload
         #:qos            [qos            'qos-2]
         #:retained       [retained       #f]
         #:properties     [properties     (create-MQTTProperties)])
  (make-MQTTClient_message
   (map char->integer '(#\M #\Q #\T #\M)) ; eyecatcher is fixed
   1                                      ; struct-version is fixed
   (bytes-length payload)
   payload
   qos
   retained
   #f                                     ; server sets dup flag
   0                                      ; msgid is used only internally
   properties))





;;============================================================
;; FFI
;;============================================================

(define-ffi-definer define-mqtt-client (ffi-lib "libpaho-mqtt3c" '("1.3.9" "1" #f)))

(define-mqtt-client MQTTClient_destroy
  (_fun (_ptr i _MQTTClient-pointer) ; handle
        -> _void)
  #:wrap (deallocator))

(define-mqtt-client MQTTClient_create
  (_fun (handle : (_ptr o _MQTTClient-pointer))  ; handle
        _string/utf-8                            ; serverURI
        _string/utf-8                            ; clientId
        _persistence-type                        ; persistence_type
        (_cpointer/null 'persistence-context)    ; persistence_context
        -> (r : _int)
        -> (begin
             (check r 'mqtt/create)
             handle))
  #:wrap (allocator MQTTClient_destroy))

(define-mqtt-client MQTTClient_connect
  (_fun _MQTTClient-pointer                ; handle
        _MQTTClient_connectOptions-pointer ; options
        -> (r : _int)
        -> (check r 'mqtt/connect)))

(define-mqtt-client MQTTClient_disconnect
  (_fun _MQTTClient-pointer  ; handle
        _int                 ; timeout
        -> (r : _int)
        -> (check r 'mqtt/disconnect)))

(define-mqtt-client MQTTClient_publishMessage
  (_fun _MQTTClient-pointer                               ; handle
        _string/utf-8                                     ; topic
        _MQTTClient_message-pointer                       ; msg
        (dt : (_ptr o _MQTTClient_deliveryToken-pointer)) ; dt
        -> (r : _int)
        -> (begin
             (check r 'mqtt/publish-message)
             dt)))

(define-mqtt-client MQTTClient_waitForCompletion
  (_fun _MQTTClient-pointer                        ; handle
        (_ptr i _MQTTClient_deliveryToken-pointer) ; dt
        _ulong                                     ; timeout
        -> (r : _int)
        -> (check r 'mqtt/wait-for-completion)))

(define-mqtt-client MQTTClient_subscribe
  (_fun _MQTTClient-pointer  ; handle
        _string/utf-8        ; topic
        _qos                 ; qos
        -> (r : _int)
        -> (check r 'mqtt/subscribe)))

(define-mqtt-client MQTTClient_yield
  (_fun -> _void))

(define-mqtt-client MQTTClient_receive
  (_fun _MQTTClient-pointer                        ; handle
        (topic-name : (_ptr o _string/utf-8))      ; topic name
        (_ptr o _int)                              ; topic len
        (message : (_ptr o _MQTTClient_message-pointer/null)) ; message
        _ulong                                     ; timeout
   -> (r : _int)
   -> (begin
        (check r 'mqtt/receive)
        (values message topic-name))))
  



