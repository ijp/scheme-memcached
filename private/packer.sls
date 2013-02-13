#!r6rs
;;; Packer/Unpacker macros for bytevectors
;;; Not fully general, but sufficient for now.
(library (memcached private packer)
(export define-packer/unpacker)
(import (rnrs)
        (for (only (ijputils common) sum) expand)
        (for (memcached private packer-utils) expand)
        (for (memcached private packer-io) expand)
        (memcached private utils)
        (for (wak foof-loop) expand))

(define-syntax-case
  ((define-packer/unpacker (packer unpacker) clause clauses ...) stx)
  (begin
    (check-clauses #'(clause clauses ...))
    #'(begin
        (define packer
          (make-packer clause clauses ...))
        (define unpacker
          (make-unpacker clause clauses ...)))))

(define-syntax make-packer
  (lambda (stx)
    (define (clause-writer clause port arg)
      (syntax-case clause (u8 u16 u32 big)
        ((id u8)
         #`(put-u8 #,port #,arg))
        ((id u16 big)
         #`(put-u16 #,port #,arg (endianness big)))
        ((id u32 big)
         #`(put-u32 #,port #,arg (endianness big)))
        ((id u64 big)
         #`(put-u64 #,port #,arg (endianness big)))))

    (syntax-case stx ()
      ((make-bytevector-packer clause clauses ...)
        (let* ((clauses #'(clause clauses ...))
               (args (map clause-id clauses))
               (port (car (generate-temporaries '(port))))) ; necessary?
         #`(lambda (#,port #,@args)
             #,@(loop ((for clause (in-list clauses))
                       (for arg (in-list args))
                       (for result
                         (listing (clause-writer clause port arg))))
                      => result))))
      ((make-bytevector-packer . _)
       (syntax-violation 'make-bytevector-packer
                         "expects at least one packer clause"
                         stx)))))

(define-syntax make-unpacker
  (lambda (stx)
    (define (clause-getter clause bv idx)
      (syntax-case clause (u8 u16 u32 big)
        ((id u8)
         #`(bytevector-u8-ref #,bv #,idx))
        ((id u16 big)
         #`(bytevector-u16-ref #,bv #,idx (endianness big)))
        ((id u32 big)
         #`(bytevector-u32-ref #,bv #,idx (endianness big)))
        ((id u64 big)
         #`(bytevector-u64-ref #,bv #,idx (endianness big)))))

    (syntax-case stx ()
      ((make-unpacker clause clauses ...)
        (let* ((clauses #'(clause clauses ...))
               (vals (map clause-id clauses))
               (total-size (sum (map clause-size clauses)))
               (bv (car (generate-temporaries '(bv))))) ; necessary?
         #`(lambda (port)
             (define #,bv (get-bytevector-n port #,total-size))
             (assert (= #,total-size (bytevector-length #,bv)))
             (let (#,@(loop ((for clause (in-list clauses))
                             (for val (in-list vals))
                             (with idx 0 (+ idx (clause-size clause)))
                             (for result
                               (listing #`(#,val #,(clause-getter clause bv idx)))))
                            => result))
               (values #,@vals)))))
      ((make-bytevector-packer . _)
       (syntax-violation 'make-unpacker
                         "expects at least one packer clause"
                         stx)))))

)
