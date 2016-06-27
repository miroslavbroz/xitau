#!/usr/bin/gnuplot

set term x11

rad = 180./pi

set xl "B/lambda [cycles]"
set yl "closure phase arg T_3 [deg] (shifted by dataset number)"

set ytics 360
set mytics 2
set grid ytics mytics
set zeroaxis

load "T0.plt"
set arrow from T0-2400000,graph 0 to T0-2400000,graph 1 nohead lt 0

p \
  "closurephase.dat" u (sqrt($2**2+$3**2)/$6):($8*rad+$9*360)  t "synthetic closure phase" w p lt 7 pt 1,\
  "Closure.dat"      u (sqrt($2**2+$3**2)/$6):($10+$12*360):11 t "observed closure phase" w err lt 3 pt 1 ps 0.5,\
  "chi2_CLO.dat"     u (sqrt($2**2+$3**2)/$6):($9*rad+$11*360) t "residua" w l lt 1 lw 1,\
  "<awk '($NF+0>100)' chi2_CLO.dat" u (sqrt($2**2+$3**2)/$6):($9*rad+$11*360) t "chi^2 > 100" w p lt 1 pt 6 ps 1.5
pa -1

set term png small
set out "chi2_CLO.png"
rep



