;; A version with a slightly nicer interface, since Guile has keyword
;; arguments, and R6RS does not.
;; Maybe it would be better to have this module only be a stub, that
;; changes the bindings in the normal (memcached memcached) module.
(define-module (memcached guile)
  #:use-module ((memcached memcached)
                #:renamer (lambda (sym)
                            (define changes
                              '((memcached-set! . mc:set!)
                                (memcached-add! . mc:add!)
                                (memcached-replace! . mc:replace!)))
                            (cond ((assoc sym changes) => cdr)
                                  (else sym))))
  #:re-export (memcached-get
               memcached-delete!
               memcached-prepend!
               memcached-append!
               memcached-version
               memcached-flush!)
  #:export (memcached-set!
            memcached-add!
            memcached-replace!))

(define* (memcached-set! mc key value #:key (expires 0))
  (mc:set! mc key value expires))

(define* (memcached-add! mc key value #:key (expires 0))
  (mc:add! mc key value expires))

(define* (memcached-replace! mc key value #:key (expires 0))
  (mc:replace! mc key value expires))

