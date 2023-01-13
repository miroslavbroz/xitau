#!/usr/bin/gnuplot

set term x11
load "output.gnu"

set xl "x"
set yl "y"
set zl "z"
set cbl "-"

set cbr [0:]

set view 0,0,2.0
set view equal xyz
set xyplane 0.0
set palette gray

set arrow from 0,0,0 to s1,s2,s3 front lc 'orange'
set arrow from 0+0.02,0,0 to o1+0.01,o2,o3 front lc 'blue'

sp \
  "<./poly.awk output.poly1.01" u 4:5:6 w lp,\
  "<./poly.awk output.poly1.99" u 4:5:6 w lp,\
  "<./poly.awk output.poly1.01 | awk '($1==14) || (NF==0)'" u 4:5:6 w l lw 3 lc 'black',\

pa -1

set term png small size 1024,1024
set out "output.poly1.01.png"
rep

q

  "<./poly.awk output.poly1.99" u 4:5:6 w lp not,\
  "<./poly.awk output.poly1.49 | awk '($1==88) || (NF==0)'" u 4:5:6 w l lw 3 lc 'black',\
  "<awk '(NR>1)' output.centre" u 2:3:4:1 w labels tc 'brown' not,\

