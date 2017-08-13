#!/bin/sh
cd $(dirname $0)
erl -detached -sname owbot -pa ebin deps/*/ebin -s owbot
