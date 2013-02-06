#!r6rs
(library (memcached private utils)
(export define-syntax-case)
(import (rnrs))

(define-syntax define-syntax-case
  (syntax-rules ()
    ((define-syntax-case ((binding macro-arg ...) stx) template)
     (define-syntax binding
       (lambda (stx)
         (syntax-case stx ()
           ((binding macro-arg ...)
            template)))))))

)
