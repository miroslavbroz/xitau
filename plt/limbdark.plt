#!/usr/bin/gnuplot

set term x11

nm = 1.e-9

set xl "lambda [nm]"
set yl "u_limb []"

set yr [0:]

p \
  "<awk '($1==1)' limbdark.dat" u ($2/nm):3 w p,\
  "<awk '($1==2)' limbdark.dat" u ($2/nm):3 w p,\
  "<awk '($1==3)' limbdark.dat" u ($2/nm):3 w p,\
  "<awk '($1==4)' limbdark.dat" u ($2/nm):3 w p,\

pa -1

