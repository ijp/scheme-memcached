#!r6rs
(library (memcached private utils)
(export define-syntax-case alist->hashtable)
(import (rnrs))

(define-syntax define-syntax-case
  (syntax-rules ()
    ((define-syntax-case ((binding macro-arg ...) stx) template)
     (define-syntax binding
       (lambda (stx)
         (syntax-case stx ()
           ((binding macro-arg ...)
            template)))))))

(define alist->hashtable
  (letrec ((a->h (lambda (a h)
                   (for-each (lambda (pair)
                               (hashtable-set! h (car pair) (cdr pair)))
                             a)
                   h)))
    (case-lambda
      ((alist)
       (a->h alist (make-eqv-hashtable)))
      ((alist hash-function equiv)
       (a->h alist (make-hashtable hash-function equiv))))))

)
