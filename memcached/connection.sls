#!r6rs
;; Copyright (C) 2013 Ian Price <ianprice90@googlemail.com>
;;
;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.
;;
;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;; Notes:
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
