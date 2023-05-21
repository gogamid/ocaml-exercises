#!/bin/bash

eval $(opam env)
#echo -e "#use \"exercises3-14.ml\";;\n" | utop
echo -e "$1\n" | utop