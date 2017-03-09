#!/bin/sh
make

if [ $? -eq 0 ]; then
  erl -pa ebin deps/*/ebin -s owbot
fi
