#!/usr/bin/env bash

#set -x

if [ x$(uname) == x"Darwin" ]; then
BASENAME=basename
else
BASENAME="basename -a"
fi

UNIQ=$(find test/should_pass test/should_fail test/known_problems/*/ -name \*.erl \
        | xargs $BASENAME \
        | sort \
        | uniq -c \
        | sort -n \
        | tail -1)

COUNT=$(echo $UNIQ | awk '{print $1}')

if [ "$COUNT" == 1 ]; then
    exit 0
else
    echo "Name clash in tests:"
    find test -name $(echo $UNIQ | awk '{print $2}')
    exit 1
fi
