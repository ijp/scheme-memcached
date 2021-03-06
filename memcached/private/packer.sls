#!r6rs
;; Copyright (C) 2013 Ian Price <ianprice90@googlemail.com>
;;
;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.
;;
;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Notes:
;;; Packer/Unpacker macros for bytevectors
;;; Not fully general, but sufficient for now.
(library (memcached private packer)
(export define-packer/unpacker)
(import (rnrs)
        (for (memcached private packer-utils) expand)
        (for (memcached private packer-io) expand)
        (memcached private utils)
        (for (rnrs lists) expand))

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
             #,@(fold-right (lambda (clause arg rest)
                              (cons (clause-writer clause port arg) rest))
                            '()
                            clauses
                            args))))
      ((make-bytevector-packer . _)
       (syntax-violation 'make-bytevector-packer
                         "expects at least one packer clause"
                         stx)))))

(define-syntax make-unpacker
  (lambda (stx)
    (define (clause-reader clause port)
      (syntax-case clause (u8 u16 u32 big)
        ((id u8)
         #`(get-u8 #,port))
        ((id u16 big)
         #`(get-u16 #,port (endianness big)))
        ((id u32 big)
         #`(get-u32 #,port (endianness big)))
        ((id u64 big)
         #`(get-u64 #,port (endianness big)))))

    (syntax-case stx ()
      ((make-unpacker clause clauses ...)
        (let* ((clauses #'(clause clauses ...))
               (vals (map clause-id clauses))
               (port (car (generate-temporaries '(port))))) ; necessary?
         #`(lambda (#,port)
             (let* (#,@(fold-right (lambda (clause val rest)
                              (cons #`(#,val #,(clause-reader clause port)) rest))
                            '()
                            clauses
                            vals))
               (cond ((memp eof-object? (list #,@vals)) =>
                      (lambda (rest)
                        (error 'make-unpacker "eof early" rest)))
                     (else (values #,@vals)))))))
      ((make-bytevector-packer . _)
       (syntax-violation 'make-unpacker
                         "expects at least one packer clause"
                         stx)))))

)
