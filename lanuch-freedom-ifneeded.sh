#!/bin/sh

count=`ps aux | grep Freedom | grep -v grep | wc -l`

if test $count -lt 3; then
    open /Applications/Freedom.app
fi


