#!/bin/bash

erlc main.erl && cat input-full.txt | erl -noshell -s main start -s init stop

