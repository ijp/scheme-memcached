#!r6rs
;; For now, only a single memcached connection. Later, you will be
;; able to pool a bunch of connections together.
(library (memcached connection)
(export make-memcached-connection
        connection?
        connection-close
        connection-input-port
        connection-output-port)
(import (rnrs)
        (only (guile) getaddrinfo socket connect close addrinfo:addr
              addrinfo:fam addrinfo:socktype addrinfo:protocol))

(define-record-type connection
  (fields input-port output-port))

(define make-memcached-connection
  (case-lambda
    (()
     (make-memcached-connection "localhost" 11211))
    ((host port)
     (let* ((ai (car (getaddrinfo host (number->string port))))
            (sock (socket (addrinfo:fam ai)
                          (addrinfo:socktype ai)
                          (addrinfo:protocol ai))))
       (connect sock (addrinfo:addr ai))
       (make-connection sock sock)))))

(define (connection-close conn)
  (close (connection-input-port conn)))

)
