#!/bin/bash
# Copyright (C) 2013 Ian Price <ianprice90@googlemail.com>
#
# This program is free software, you can redistribute it and/or
# modify it under the terms of the new-style BSD license.
#
# You should have received a copy of the BSD license along with this
# program. If not, see <http://www.debian.org/misc/bsd.license>.

# Notes:

# Expected to be ran from the memcached/ directory

# Uses a default running memcached, so need to make sure you don't
# have an instance running on the machine at the default port

TMPFILE=$(mktemp /tmp/memcached.pid.XXXXXX)
memcached -d -P $TMPFILE
guile -L . -x .sls tests.scm
kill $(cat $TMPFILE)
