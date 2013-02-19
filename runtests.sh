#!/bin/bash

# Expected to be ran from the memcached/ directory

# Uses a default running memcached, so need to make sure you don't
# have an instance running on the machine at the default port

TMPFILE=$(mktemp /tmp/memcached.pid.XXXXXX)
memcached -d -P $TMPFILE
guile -L .. -x .sls tests.scm
kill $(cat $TMPFILE)
