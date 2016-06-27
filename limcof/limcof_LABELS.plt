#!/usr/bin/gnuplot

set term x11

nm = 1.e-9

set xl "lambda [nm]"
set yl "x_lin []"

p "<./limcof.awk 10000 4.0 +0.0 limcof.dat" u ($1/nm):5 w lp,\
  "<./limcof.awk  5000 4.0 +0.0 limcof.dat" u ($1/nm):5 w l,\
  "<./limcof.awk 10000 2.0 +0.0 limcof.dat" u ($1/nm):5 w l,\
  "<./limcof.awk 10000 4.0 -5.0 limcof.dat" u ($1/nm):5 w l,\
  "<./limcof.awk 10000 4.0 +0.0 limcof.dat" u ($1/nm):5:6 w labels font "Helvetica,16"
pa -1


