#!/usr/bin/gnuplot

set term x11

nm = 1.e-9

set xl "lambda_eff [nm]"
set yl "UBV magnitude [mag]"

set yr [:] reverse
set ytics 0.1

p \
  "chi2_SED.dat" u ($1/nm):3 t "synthetic" w p pt 1 lt 8,\
  "Sed.dat" u ($1/nm):3   t "observed" w l   lt 3,\
  "Sed.dat" u ($1/nm):3:4 not          w err lt 3 ps 0,\
  "chi2_SED.dat" u ($1/nm):3 t "residua" w l lt 1 lw 3
pa -1

set term png small
set out "chi2_SED.png"
rep

q


