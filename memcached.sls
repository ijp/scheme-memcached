#!r6rs
;; Copyright (C) 2013 Ian Price <ianprice90@googlemail.com>
;;
;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.
;;
;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.
(library (memcached)
(export make-memcached-connection
        connection-close
        memcached-get
        memcached-set!
        memcached-add!
        memcached-replace!
        memcached-delete!
        memcached-prepend!
        memcached-append!
        memcached-version
        memcached-flush!
        memcached-incr!
        memcached-decr!
        )
(import (memcached commands)
        (memcached connection))
)
