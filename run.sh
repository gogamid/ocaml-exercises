#!/bin/bash
pkill -f "utop"
eval $(opam env)
utop -init <(echo "#use \"$1\";;")
