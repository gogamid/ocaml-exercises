#!/bin/bash

eval $(opam env)
utop -init <(echo "#use \"$1\";;")
