#!/bin/bash

erl -sname enspector -pa ebin/ -pa deps/*/ebin \
  -boot start_sasl -s enspector
