#!/usr/bin/env sh
cd `dirname $0`
erl \
    -sname canvas_demo \
    -eval "application:start(nprocreg)" \
    -eval "application:start(canvas_demo)"
