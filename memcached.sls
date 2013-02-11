#!r6rs
(library (memcached memcached)
(export memcached-get
        memcached-set!
        memcached-add!
        memcached-replace!
        )
(import (rnrs)
        (memcached private packer)
        (memcached private utils)
        (memcached connection))

(define magic/request #x80)
(define magic/response #x81)

(define response-alist
  '((#x00 . "No error")
    (#x01 . "Key not found")
    (#x02 . "Key exists")
    (#x03 . "Value too large")
    (#x04 . "Invalid arguments")
    (#x05 . "Item not stored")
    (#x06 . "Incr/Decr on non-numeric value.")
    (#x81 . "Unknown command")
    (#x82 . "Out of memory")))

(define (no-error? byte)
  (= byte #x00))

(define response-message
  (let ((h (alist->hashtable response-alist)))
    (lambda (key)
      (hashtable-ref h key #f))))

(define opcode-alist
  '((#x00 . Get)
    (#x01 . Set)
    (#x02 . Add)
    (#x03 . Replace)
    (#x04 . Delete)
    (#x05 . Increment)
    (#x06 . Decrement)
    (#x07 . Quit)
    (#x08 . Flush)
    (#x09 . GetQ)
    (#x0A . No-op)
    (#x0B . Version)
    (#x0C . GetK)
    (#x0D . GetKQ)
    (#x0E . Append)
    (#x0F . Prepend)
    (#x10 . Stat)
    (#x11 . SetQ)
    (#x12 . AddQ)
    (#x13 . ReplaceQ)
    (#x14 . DeleteQ)
    (#x15 . IncrementQ)
    (#x16 . DecrementQ)
    (#x17 . QuitQ)
    (#x18 . FlushQ)
    (#x19 . AppendQ)
    (#x1A . PrependQ)))

(define opcode-name
  (let ((h (alist->hashtable opcode-alist)))
    (lambda (key)
      (hashtable-ref h key #f))))

(define opcode-byte
  (let* ((swap-cons (lambda (pair) (cons (cdr pair) (car pair))))
         (h (alist->hashtable (map swap-cons opcode-alist))))
    (lambda (key)
      (hashtable-ref h key #f))))

(define-packer/unpacker (mc-pack mc-unpack)
   (magic u8)
   (opcode u8)
   (key-len u16 big)
   (extras-len u8)
   (data-type u8)
   (status u16 big)
   (total-body-len u32 big)
   (opaque u32 big)
   (cas u64 big))

(define (write-packet out header extras key value)
  (put-bytevector out header)
  (when extras
    (put-bytevector out extras))
  (when key
    (put-bytevector out key))
  (when value
    (put-bytevector out value))
  (flush-output-port out))

(define (get-packet port)
  (let-values (((magic opcode key-len extras-len data-type
                 status total-body-len opaque cas)
                ;; TODO: fix magic value
                (mc-unpack (get-bytevector-n port 24))))
    (let* ((extra (if (zero? extras-len)
                      #f
                      (get-bytevector-n port extras-len)))
           (key   (if (zero? key-len)
                      #f
                      (get-bytevector-n port key-len)))
           (body-len (- total-body-len extras-len key-len))
           (body  (if (zero? body-len)
                      #f
                      (get-bytevector-n port body-len))))
      (values status cas extra key body))))

(define (write-get out key)
  (define key-len (bytevector-length key))
  (define extras-len 0)
  (define value-len 0)
  (define total-len (+ value-len key-len extras-len))
  (define header
    (mc-pack magic/request
             (opcode-byte 'Get)
             key-len
             extras-len
             0 ; data type 
             0 ; reserved
             total-len
             0 ; opaque
             0 ; CAS
             ))
  (write-packet out header #f key #f))

(define (memcached-get mc key)
  (write-get (connection-output-port mc) key)

  (let-values (((status cas extra key body) (get-packet (connection-input-port mc))))
    (if (no-error? status)
        body
        (error 'memcached-get (response-message status) key))))

(define (write-set out opcode key value expiration)
  (define extras-len 8)
  (define key-len (bytevector-length key))
  (define value-len (bytevector-length value))
  (define total-len (+ extras-len key-len value-len))
  (define header
    (mc-pack magic/request (opcode-byte opcode) key-len extras-len 0 0 total-len 0 0))
  (define extras (make-bytevector extras-len 0))
  (bytevector-u16-set! extras 4 expiration (endianness big))
  (write-packet out header extras key value))

(define-syntax-rule (define-setter name opcode)
  (define name
    (let ((set (lambda (mc key value expiration)
                 (let ((i (connection-input-port mc))
                       (o (connection-output-port mc)))
                   (write-set o opcode key value expiration)
                   (let-values (((status cas extra key body) (get-packet i)))
                     (if (no-error? status)
                         body
                         (error 'name (response-message status) key)))))))
      (case-lambda
        ((mc key value)
         (set mc key value 0))
        ((mc key value expiration)
         (set mc key value expiration))))))

(define-setter memcached-set! 'Set)
(define-setter memcached-add! 'Add)
(define-setter memcached-replace! 'Replace)

)
