#!/bin/bash

erl -sname enspector -pa ebin/ -pa $HOME/src/cowboy/ebin/ \
  -boot start_sasl -s enspector
