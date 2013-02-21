#!r6rs
;; Copyright (C) 2013 Ian Price <ianprice90@googlemail.com>
;;
;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.
;;
;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.
(import (rnrs)
        (wak trc-testing)
        (memcached)
        (srfi :39 parameters)
        (only (memcached private utils) define-syntax-rule))

(define current-connection (make-parameter #f))
(define u utf8->string)
(define s string->utf8)

(define-syntax-rule (test-error pred form)
  (test-eqv #t (guard (exn ((pred exn) #t)
                           (else #f))
                 form
                 #f)))

(define-syntax-rule (test-not-error form)
  (test-eqv #t (guard (exn (else #f)) form #t)))

(define-syntax-rule (test-not form)
  (test-eqv #f form))

(define (setup!)
    (memcached-set! (current-connection) (s "foo") (s "bar"))
    (memcached-set! (current-connection) (s "baz") (s "quux"))
    (memcached-set! (current-connection) (s "zot") (s "frotz")))

(define (cleanup!)
  (memcached-flush! (current-connection)))

(define-syntax-rule (define-test-case* suite name form forms ...)
  ;; trc testing doesn't do suite-level setup and teardown
  ;; NB: it adds 2 to the number of tests passed for each case
  (define-test-case suite name
    (test-case name () (setup!) form forms ... (cleanup!))))

(define-test-suite memcached-tests
  "All tests for library")

(define-test-case* memcached-tests mc:get
  (test-equal (s "bar") (memcached-get (current-connection) (s "foo")))
  (test-equal (s "quux") (memcached-get (current-connection) (s "baz")))
  (test-equal (s "frotz") (memcached-get (current-connection) (s "zot")))
  (test-not (memcached-get (current-connection) (s "notexists"))))


(define-test-case* memcached-tests mc:set!
  (test-not-error (memcached-set! (current-connection) (s "foo") (s "newbar")))
  (test-not-error (memcached-set! (current-connection) (s "xyzzy") (s "asdf")))
  (test-equal (s "newbar") (memcached-get (current-connection) (s "foo")))
  (test-equal (s "quux") (memcached-get (current-connection) (s "baz")))
  (test-equal (s "asdf") (memcached-get (current-connection) (s "xyzzy"))))

(define-test-case* memcached-tests mc:add!
  (test-error error? (memcached-add! (current-connection) (s "foo") (s "newfoo")))
  (test-not-error (memcached-add! (current-connection) (s "notexists") (s "new")))
  (test-equal (s "new") (memcached-get (current-connection) (s "notexists"))))

(define-test-case* memcached-tests mc:replace!
  (test-not-error (memcached-replace! (current-connection) (s "foo") (s "newfoo")))
  (test-equal (s "newfoo") (memcached-get (current-connection) (s "foo")))
  (test-error error? (memcached-replace! (current-connection) (s "xyzzy") (s "foo"))))

(define-test-case* memcached-tests mc:delete!
  (test-not-error (memcached-get (current-connection) (s "foo")))
  (test-not-error (memcached-delete! (current-connection) (s "foo")))
  (test-not (memcached-get (current-connection) (s "foo")))
  (test-not-error (memcached-delete! (current-connection) (s "notexists"))))

(define-test-case* memcached-tests mc:prepend/append!
  (test-error error? (memcached-prepend! (current-connection) (s "notexists") (s "prefix:")))
  (test-error error? (memcached-append! (current-connection) (s "notexists") (s ":suffix")))
  (test-not-error (memcached-append! (current-connection) (s "foo") (s ":suffix")))
  (test-equal (s "bar:suffix") (memcached-get (current-connection) (s "foo")))
  (test-not-error (memcached-prepend! (current-connection) (s "baz") (s "prefix:")))
  (test-equal (s "prefix:quux") (memcached-get (current-connection) (s "baz"))))

(define-test-case* memcached-tests mc:flush!
  (test-not-error (memcached-flush! (current-connection)))
  (test-not (memcached-get (current-connection) (s "foo")))
  (test-not (memcached-get (current-connection) (s "baz")))
  (test-not (memcached-get (current-connection) (s "zot"))))

(define-test-case* memcached-tests mc:version
  (test-predicate string? (memcached-version (current-connection))))

(define-test-case* memcached-tests mc:incr/decr!
  ;; TODO:
  (test-eqv #t #t))

(parameterize ((current-connection (make-memcached-connection)))
 (run-test memcached-tests))
