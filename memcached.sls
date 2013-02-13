#!r6rs
(library (memcached memcached)
(export memcached-get
        memcached-set!
        memcached-add!
        memcached-replace!
        memcached-delete!
        memcached-prepend!
        memcached-append!
        memcached-version
        memcached-flush!
        memcached-incr!
        memcached-decr!
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

(define (write-packet-body out extras key value)
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
                (mc-unpack port)))
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

(define (write-key-only out opcode key)
  (define key-len (bytevector-length key))
  (define extras-len 0)
  (define value-len 0)
  (define total-len (+ value-len key-len extras-len))
  (mc-pack out
           magic/request
           (opcode-byte opcode)
           key-len
           extras-len
           0 ; data type 
           0 ; reserved
           total-len
           0 ; opaque
           0 ; CAS
           )
  (write-packet-body out #f key #f))

(define-syntax-rule (define-command/key-only name opcode)
  (define (name mc key)
    (let ((i (connection-input-port mc))
          (o (connection-output-port mc)))
      (write-key-only o opcode key)
      (let-values (((status cas extra key body) (get-packet i)))
        (if (no-error? status)
            body
            (error 'name (response-message status) key))))))

(define-command/key-only memcached-get 'Get)
(define-command/key-only memcached-delete! 'Delete)

(define (write-set out opcode key value expiration)
  (define extras-len 8)
  (define key-len (bytevector-length key))
  (define value-len (bytevector-length value))
  (define total-len (+ extras-len key-len value-len))
  (define extras (make-bytevector extras-len 0))
  (bytevector-u16-set! extras 4 expiration (endianness big))
  (mc-pack out magic/request (opcode-byte opcode) key-len extras-len 0 0 total-len 0 0)
  (write-packet-body out extras key value))

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

(define (write-key+value out opcode key value)
  (define extras-len 0)
  (define key-len (bytevector-length key))
  (define value-len (bytevector-length value))
  (define total-len (+ extras-len key-len value-len))
  (mc-pack out magic/request (opcode-byte opcode) key-len extras-len 0 0 total-len 0 0)
  (write-packet-body out #f key value))

(define-syntax-rule (define-command/key+value name opcode)
  (define (name mc key value)
    (let ((i (connection-input-port mc))
          (o (connection-output-port mc)))
      (write-key+value o opcode key value)
      (let-values (((status cas extra key body) (get-packet i)))
        (if (no-error? status)
            body
            (error 'name (response-message status) key))))))

(define-command/key+value memcached-append! 'Append)
(define-command/key+value memcached-prepend! 'Prepend)

(define (write-no-args out opcode)
  (mc-pack out magic/request (opcode-byte opcode) 0 0 0 0 0 0 0)
  (write-packet-body out #f #f #f))

(define-syntax define-command/no-args
  (syntax-rules ()
    ((define-command/no-args name opcode)
     (define-command/no-args name opcode (lambda (x) x)))
    ((define-command/no-args name opcode f)
     (define (name mc)
       (let ((i (connection-input-port mc))
             (o (connection-output-port mc)))
         (write-no-args o opcode)
         (let-values (((status cas extra key body) (get-packet i)))
           (if (no-error? status)
               (f body)
               (error 'name (response-message status) key))))))))


(define-command/no-args memcached-version 'Version utf8->string) ;?
(define-command/no-args memcached-flush! 'Flush)

(define (write-incr out opcode key expiration by initial)
  (define extras-len 20)
  (define key-len (bytevector-length key))
  (define value-len 0)
  (define total-len (+ extras-len key-len value-len))
  (define extras (make-bytevector extras-len 0))
  (bytevector-u64-set! extras 0 by (endianness big))
  (bytevector-u64-set! extras 8 initial (endianness big))
  (bytevector-u16-set! extras 16 expiration (endianness big))
  (mc-pack out magic/request (opcode-byte opcode) key-len extras-len 0 0 total-len 0 0)
  (write-packet-body out extras key #f))

(define no-initial #xffffffff)

(define-syntax-rule (define-command/incr name opcode)
  (define name
    (let ((set (lambda (mc key expiration by initial)
                 (let ((i (connection-input-port mc))
                       (o (connection-output-port mc)))
                   (write-incr o opcode key expiration by initial)
                   (let-values (((status cas extra key body) (get-packet i)))
                     (if (no-error? status)
                         (bytevector-u64-ref body 0 (endianness big))
                         (error 'name (response-message status) key)))))))
      (case-lambda
        ;; TODO: Not entirely convinced by the ordering. Might be more
        ;; useful to have the "by" and "initial" arguments come before
        ;; the expiration
        ((mc key)
         (set mc key 0 0 no-initial))
        ((mc key expiration)
         (set mc key expiration 0 no-initial))
        ((mc key expiration by)
         (set mc key expiration by no-initial))
        ((mc key expiration by initial)
         (set mc key expiration by initial))))))

(define-command/incr memcached-incr! 'Increment)
(define-command/incr memcached-decr! 'Decrement)

)
