#!/usr/bin/gnuplot

set term x11

nm = 1.e-9

set xl "lambda [nm]"
set yl "V^2 [] (shifted by dataset number)"

set ytics 5
set mytics 5
set grid ytics mytics
set zeroaxis

load "T0.plt"
set arrow from T0-2400000,graph 0 to T0-2400000,graph 1 nohead lt 0

p \
  "visibility.dat" u ($4/nm):($5+$6)   t "synthetic visibility" w p lt 7 pt 1,\
  "Vis.dat"        u ($4/nm):($6+$8):7 t "observed visibility" w err lt 3 pt 1 ps 0.5,\
  "chi2_VIS.dat"   u ($4/nm):($5+$7)   t "residua" w l lt 1 lw 1,\
  "<awk '($NF+0>100)' chi2_VIS.dat" u ($4/nm):($5+$7) t "chi^2 > 100" w p lt 1 pt 6 ps 1.5
pa -1

set term png small
set out "chi2_VIS_LAMBDA.png"
rep



