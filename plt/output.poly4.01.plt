#!/usr/bin/gnuplot

set term x11
load "output.gnu"

set xl "x"
set yl "y"
set zl "z"
set cbl "-"

set cbr [0:]

set view 0,0
set view equal xyz
set xyplane 0.0
set palette gray

set arrow from 0,0,0 to s1__,s2__,s3__ front lc 'orange'
set arrow from 0+0.01,0,0 to o1__+0.01,o2__,o3__ front lc 'blue'

sp \
  "<./poly.awk output.poly4.01" u 4:5:6 w lp not,\
  "<./poly.awk output.poly5.01" u 4:5:6 w lp not,\
  "<./poly.awk output.poly4.01 | awk '($1==88) || (NF==0)'" u 4:5:6 w lp lw 3 lc 'black',\
  "<./poly.awk output.poly5.01 | awk '($1==88) || (NF==0)'" u 4:5:6 w lp lw 1 lc 'gray',\

pa -1

q


