#!r6rs
(library (memcached private packer-io)
(export get-u16
        get-u32
        get-u64
        )
(import (rnrs base)
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

)
