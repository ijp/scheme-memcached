#!r6rs
(library (memcached private packer-utils)
(export check-clauses clause-size clause-id)
(import (rnrs))

(define (check-clauses _) #t) ; TODO: implement

(define (clause-size clause)
  (syntax-case clause (u8 u16 u32 big)
    ((id u8) 1)
    ((id u16 big) 2)
    ((id u32 big) 4)))

;; (define (clause-setter clause bv idx val)
;;   (syntax-case clause (u8 u16 u32 big)
;;     ((id u8)
;;      #`(bytevector-u8-set! #,bv #,idx #,val))
;;     ((id u16 big)
;;      #`(bytevector-u16-set! #,bv #,idx #,val big))
;;     ((id u32 big)
;;      #`(bytevector-u32-set! #,bv #,idx #,val big))))

(define (clause-id clause)
  (syntax-case clause ()
    ((id . _) #'id)))
)

