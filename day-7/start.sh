#!/bin/bash

erlc main.erl && cat input-full.txt | erl -noshell -s main start -s init stop
erlc main2.erl && cat input-full.txt | erl -noshell -s main2 start -s init stop

