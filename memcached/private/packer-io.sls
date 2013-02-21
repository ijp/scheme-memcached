#!r6rs
;; Copyright (C) 2013 Ian Price <ianprice90@googlemail.com>
;;
;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.
;;
;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.
(library (memcached private packer-io)
(export get-u16
        get-u32
        get-u64
        put-u16
        put-u32
        put-u64
        )
(import (rnrs base)
        (rnrs bytevectors)
        (rnrs io ports))

(define (get-u16 port endianness)
  (define bv (make-bytevector 2))
  (define ans (get-bytevector-n! port bv 0 2))
  (cond ((eof-object? ans) ans)
        ((= ans 2) (bytevector-u16-ref bv 0 endianness))
        (else (error 'get-u16-big "Incomplete read"))))

(define (get-u32 port endianness)
  (define bv (make-bytevector 4))
  (define ans (get-bytevector-n! port bv 0 4))
  (cond ((eof-object? ans) ans)
        ((= ans 4) (bytevector-u32-ref bv 0 endianness))
        (else (error 'get-u32-big "Incomplete read"))))

(define (get-u64 port endianness)
  (define bv (make-bytevector 8))
  (define ans (get-bytevector-n! port bv 0 8))
  (cond ((eof-object? ans) ans)
        ((= ans 8) (bytevector-u64-ref bv 0 endianness))
        (else (error 'get-u64-big "Incomplete read"))))

(define (put-u16 port val endianness)
  (define bv (make-bytevector 2))
  (bytevector-u16-set! bv 0 val endianness)
  (put-bytevector port bv))

(define (put-u32 port val endianness)
  (define bv (make-bytevector 4))
  (bytevector-u32-set! bv 0 val endianness)
  (put-bytevector port bv))

(define (put-u64 port val endianness)
  (define bv (make-bytevector 8))
  (bytevector-u64-set! bv 0 val endianness)
  (put-bytevector port bv))

)
