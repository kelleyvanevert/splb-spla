#!/bin/bash
llc ${1%.*}.ll -o ${1%.*}.s
gcc  ${1%.*}.s -o ${1%.*}.out
./${1%.*}.out
