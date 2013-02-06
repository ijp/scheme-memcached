#!r6rs
;;; Packer/Unpacker macros for bytevectors
;;; Not fully general, but sufficient for now.
(library (memcached private packer)
(export define-packer/unpacker)
(import (rnrs)
        (for (only (ijputils common) sum) expand)
        (for (memcached private packer-utils) expand)
        (memcached private utils)
        (for (wak foof-loop) expand))

(define-syntax-case
  ((define-packer/unpacker (packer unpacker) clause clauses ...) stx)
  (begin
    (check-clauses #'(clause clauses ...))
    #'(begin
        (define packer
          (make-packer clause clauses ...))
        ;; (define unpacker
        ;;   (make-unpacker clause clauses ...))
        )))

(define-syntax make-packer
  (lambda (stx)
    (define (clause-setter clause bv idx val)
      (syntax-case clause (u8 u16 u32 big)
        ((id u8)
         #`(bytevector-u8-set! #,bv #,idx #,val))
        ((id u16 big)
         #`(bytevector-u16-set! #,bv #,idx #,val (endianness big)))
        ((id u32 big)
         #`(bytevector-u32-set! #,bv #,idx #,val (endianness big)))))

    (syntax-case stx ()
      ((make-bytevector-packer clause clauses ...)
        (let* ((clauses #'(clause clauses ...))
               (args (map clause-id clauses))
               (total-size (sum (map clause-size clauses)))
               (bv (car (generate-temporaries '(bv))))) ; necessary?
         #`(lambda #,args
             (let ((#,bv (make-bytevector #,total-size)))
               #,@(loop ((for clause (in-list clauses))
                         (for arg (in-list args))
                         (with idx 0 (+ idx (clause-size clause)))
                         (for result
                           (listing (clause-setter clause bv idx arg))))
                        => result)
               #,bv))))
      ((make-bytevector-packer . _)
       (syntax-violation 'make-bytevector-packer
                         "expects at least one packer clause"
                         stx)))))
)