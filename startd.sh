#!/bin/sh
erl -detached -sname owbot -pa ebin deps/*/ebin -s owbot
