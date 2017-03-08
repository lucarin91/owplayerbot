#!/bin/sh
make

if [ $? -eq 0 ]; then
  erl -pa ebin deps/mochiweb_xpath/ebin deps/mochiweb/ebin deps/jiffy/ebin -s owbot
fi
