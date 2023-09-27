#lang racket/base


;;============================================================
;; Require and Provide
;;============================================================

(require

  (for-syntax (only-in syntax/parse
                       syntax-parse)
              (only-in racket/base
                       syntax))
  
  (only-in ffi/unsafe
           _cpointer
           _cpointer/null
           _enum
           _array/list
           _byte
           _string/utf-8
           _int
           _bool
           _fun
           _ptr
           _void
           _ulong
           cpointer?
           define-cstruct
           ffi-lib)
  
  (only-in ffi/unsafe/define
           define-ffi-definer)
  
  (only-in ffi/unsafe/alloc
           allocator
           deallocator)
  
  (only-in racket/match
           define/match))
  


(provide mqtt/will-options
         (rename-out [will-options-topicName mqtt/will-options-topic]
                     [will-options-message   mqtt/will-options-payload]
                     [will-options-retained  mqtt/will-options-retained]
                     [will-options-qos       matt/will-options-qos])
         mqtt/connect-options
         (rename-out [connect-options-keepAliveInterval mqtt/connect-options-keep-alive-interval]
                     [connect-options-cleansession      mqtt/connect-options-clean-session]
                     [connect-options-reliable          mqtt/connect-options-reliable]
                     [connect-options-will              mqtt/connect-options-will]
                     [connect-options-username          mqtt/connect-options-username]
                     [connect-options-connectTimeout    mqtt/connect-options-connect-timeout]
                     [connect-options-retryInterval     mqtt/connect-options-retry-interval])
         mqtt/message
         (rename-out [message-payload  mqtt/message-payload]
                     [message-qos      mqtt/message-qos]
                     [message-retained mqtt/message-retained]
                     [message-dup      mqtt/message-dup])
         mqtt/create-inmem
         mqtt/create-fs
         mqtt/destroy
         mqtt/connect
         mqtt/disconnect
         mqtt/publish-message
         mqtt/wait-for-completion
         mqtt/subscribe
         mqtt/yield
         mqtt/receive)


;;============================================================
;; Error Handling
;;============================================================

(struct exn:fail:mqtt exn:fail (who)
  #:transparent)

(define-syntax (mqtt-error stx)
  (syntax-parse stx
    [(_ who reason) #'(raise (exn:fail:mqtt reason (current-continuation-marks) who))]))

(define/match (check v who)
  [(0   _)   (void)]
  [(-1  who) (mqtt-error who "failure")]
  [(-2  who) (mqtt-error who "persistence error")]
  [(-3  who) (mqtt-error who "disconnected")]
  [(-4  who) (mqtt-error who "maximum messages in flight")]
  [(-5  who) (mqtt-error who "bad UTF-8 string")]
  [(-6  who) (mqtt-error who "null parameter")]
  [(-7  who) (mqtt-error who "topicname truncated")]
  [(-8  who) (mqtt-error who "bad-structure")]
  [(-9  who) (mqtt-error who "bad QOS")]
  [(-10 who) (mqtt-error who "SSL not supported")]
  [(-11 who) (mqtt-error who "bad MQTT version")]
  [(-14 who) (mqtt-error who "bad protocol")]
  [(-15 who) (mqtt-error who "bad MQTT option")]
  [(-16 who) (mqtt-error who "wrong MQTT version")]
  [(-17 who) (mqtt-error who "0-length will topic")]
  [((? exact-integer? n) who) (mqtt-error who (number->string n))])



;;============================================================
;; Data Types
;;============================================================

(define _mqtt-client-pointer              (_cpointer 'mqtt-client))
(define _delivery-token-pointer           (_cpointer 'delivery-token))
(define _ssl-options-pointer/null         (_cpointer/null 'ssl-options))
(define _name-value-pointer/null          (_cpointer/null 'name-value))
(define _data-pointer/null                (_cpointer/null 'data))
(define _persistence-context-pointer/null (_cpointer/null 'persistence-context))
(define _server-uris-pointer/null         (_cpointer/null 'server-uris))
(define _mqtt-property-pointer/null       (_cpointer/null 'mqtt-property))

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

(define _struct-id
  (_array/list _byte 4))


;; payload
;;------------------------------------------------------------

(define-cstruct _payload
  ([len  _int]
   [data _data-pointer/null]))
  


;; will
;;------------------------------------------------------------

(define-cstruct _will-options
  ([struct_id      _struct-id]
   [struct_version _int]
   [topicName      _string/utf-8]
   [message        _string/utf-8]
   [retained       _bool]
   [qos            _qos]
   [payload        _payload]))

(define (mqtt/will-options
         topic
         message
         #:retained [retained #f]
         #:qos      [qos 'qos-0])
  (make-will-options
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
   [data _data-pointer/null]))
  
;; connect-options
;;------------------------------------------------------------

(define-cstruct _connect-options
  ([struct_id           _struct-id]
   [struct_version      _int]
   [keepAliveInterval   _int]
   [cleansession        _bool]
   [reliable            _int]
   [will                _will-options-pointer/null]
   [username            _string/utf-8]
   [password            _string/utf-8]
   [connectTimeout      _int]
   [retryInterval       _int]
   [ssl                 _ssl-options-pointer/null]
   [serverURIcount      _int]
   [serverURIs          _server-uris-pointer/null]
   [MQTTVersion         _mqtt-version]
   [returned            _returned]
   [binarypwd           _binarypwd]
   [maxInflightMessages _int]
   [cleanstart          _bool]
   [httpHeaders         _name-value-pointer/null]
   [httpProxy           _string/utf-8]
   [httpsProxy          _string/utf-8]))

(define (mqtt/connect-options
         #:keep-alive-interval   [keep-alive-interval   60] ; time [s]
         #:clean-session         [clean-session         #t]
         #:reliable              [reliable               1]
         #:will                  [will                  #f]
         #:username              [username              #f]
         #:password              [password              #f]
         #:connect-timeout       [connect-timeout       30] ; time [s]
         #:retry-interval        [retry-interval         0] ; time [s] 0 means no retries
         #:ssl                   [ssl                   #f]
         #:server-uri-count      [server-uri-count       0]
         #:server-uris           [server-uris           #f]
         #:mqtt-version          [mqtt-version          'mqtt-version-default]
         #:binarypwd             [binarypwd             (make-binarypwd 0 #f)]
         #:max-inflight-messages [max-inflight-messages -1]
         #:clean-start           [clean-start           #f]
         #:http-headers          [http-headers          #f]
         #:http-proxy            [http-proxy            #f]
         #:https-proxy           [https-proxy           #f])
  (make-connect-options
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
   ssl
   server-uri-count
   server-uris
   mqtt-version
   (make-returned #f 0 0)                 ; server sets returned
   binarypwd
   max-inflight-messages
   clean-start
   http-headers
   http-proxy
   https-proxy))


;; mqtt-properties
;;------------------------------------------------------------

(define-cstruct _mqtt-properties
  ([count     _int]
   [max_count _int]
   [length    _int]
   [array     _mqtt-property-pointer/null]))

(define (create-mqtt-properties
         #:count     [count      0]
         #:max-count [max-count  0]
         #:length    [length     0]
         #:array     [array     #f])
  (make-mqtt-properties count max-count length array))

;; message
;;------------------------------------------------------------

(define-cstruct _message
  ([struct_id      _struct-id]
   [struct_version _int]
   [payloadlen     _int]
   [payload        _string/utf-8]
   [qos            _qos]
   [retained       _bool]
   [dup            _bool]
   [msgid          _int]
   [properties     _mqtt-properties]))

(define (mqtt/message
         payload
         #:qos            [qos            'qos-0]
         #:retained       [retained       #f]
         #:properties     [properties     (create-mqtt-properties)])
  (make-message
   (map char->integer '(#\M #\Q #\T #\M)) ; eyecatcher is fixed
   1                                      ; struct-version is fixed
   (string-length payload)
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

(define-mqtt-client mqtt/destroy
  (_fun (_ptr i _mqtt-client-pointer) ; handle
        -> _void)
  #:c-id MQTTClient_destroy
  #:wrap (deallocator))


(define-mqtt-client MQTTClient_create
  (_fun (handle : (_ptr o _mqtt-client-pointer)) ; handle
        _string/utf-8                            ; serverURI
        _string/utf-8                            ; clientId
        _persistence-type                        ; persistence_type
        _persistence-context-pointer/null        ; persistence_context
        -> (r : _int)
        -> (begin
             (check r 'mqtt/create)
             handle))
  #:wrap (allocator mqtt/destroy))

(define (mqtt/create-inmem server-uri client-id)
  (MQTTClient_create server-uri client-id 'persistence-none #f))

(define (mqtt/create-fs
         server-uri
         client-id
         #:persist-dir [persist-dir #f])
  (MQTTClient_create server-uri client-id 'persistence-default persist-dir))



(define-mqtt-client mqtt/connect
  (_fun _mqtt-client-pointer     ; handle
        _connect-options-pointer ; options
        -> (r : _int)
        -> (check r 'mqtt/connect))
  #:c-id MQTTClient_connect)

(define-mqtt-client mqtt/disconnect
  (_fun _mqtt-client-pointer ; handle
        _int                 ; timeout
        -> (r : _int)
        -> (check r 'mqtt/disconnect))
  #:c-id MQTTClient_disconnect)


(define-mqtt-client mqtt/publish-message
  (_fun _mqtt-client-pointer                    ; handle
        _string/utf-8                           ; topic
        _message-pointer                        ; msg
        (dt : (_ptr o _delivery-token-pointer)) ; dt
        -> (r : _int)
        -> (begin
             (check r 'mqtt/publish-message)
             dt))
  #:c-id MQTTClient_publishMessage)

(define-mqtt-client mqtt/wait-for-completion
  (_fun _mqtt-client-pointer             ; handle
        (_ptr i _delivery-token-pointer) ; dt
        _ulong                           ; timeout
        -> (r : _int)
        -> (check r 'mqtt/wait-for-completion))
  #:c-id MQTTClient_waitForCompletion)


(define-mqtt-client MQTTClient_subscribe
  (_fun _mqtt-client-pointer ; handle
        _string/utf-8        ; topic
        _qos                 ; qos
        -> (r : _int)
        -> (check r 'mqtt/subscribe)))

(define (mqtt/subscribe handle topic #:qos [qos 'qos-2])
  (MQTTClient_subscribe handle topic qos))


(define-mqtt-client mqtt/yield
  (_fun -> _void)
  #:c-id MQTTClient_yield)


(define-mqtt-client mqtt/receive
  (_fun _mqtt-client-pointer                       ; handle
        (topic-name : (_ptr o _string/utf-8))      ; topic name
        (_ptr o _int)                              ; topic len
        (message : (_ptr o _message-pointer/null)) ; message
        _ulong                                     ; timeout
   -> (r : _int)
   -> (begin
        (check r 'mqtt/receive)
        (values message topic-name)))
  #:c-id MQTTClient_receive)
  



