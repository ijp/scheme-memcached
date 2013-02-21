#!r6rs
;; Copyright (C) 2013 Ian Price <ianprice90@googlemail.com>
;;
;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.
;;
;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.
(library (memcached private packer-utils)
(export check-clauses clause-size clause-id)
(import (rnrs))

(define (check-clauses _) #t) ; TODO: implement

(define (clause-size clause)
  (syntax-case clause (u8 u16 u32 big)
    ((id u8) 1)
    ((id u16 big) 2)
    ((id u32 big) 4)
    ((id u64 big) 8)))

(define (clause-id clause)
  (syntax-case clause ()
    ((id . _) #'id)))
)

