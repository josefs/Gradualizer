#!/usr/bin/env bash

set -euo pipefail
#set -x

pushd `dirname $0` > /dev/null 2>&1
BASEDIR=`pwd`
popd > /dev/null 2>&1

if [ x`uname` = x"Darwin" ]; then
    SED=gsed
else
    SED=sed
fi

$BASEDIR/gradualizer $@ | $SED '/\(.*on line \([0-9]*\)\)/ {s//\2:\1/}' | sort -n | uniq
