#!/usr/bin/gnuplot

set size ratio -1

p \
  "test.in" u 1:2 w lp lw 3 lc 'gray',\
  "test.out" u 1:2 w lp lw 3 lc 'black',\

pa -1

set term png small
set out "test.png"
rep


