#!/bin/bash
cd /Users/oubiwann/lab/erlsci/devout

erl -pa $(rebar3 path) \
    -eval "devout:start()" \
    -noshell
