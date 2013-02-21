#!r6rs
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
(import (memcached memcached)
        (memcached connection))
)
