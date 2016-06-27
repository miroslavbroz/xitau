#!/usr/bin/gnuplot

set term x11

nm = 1.e-9  # m
shift = 0.25

set xl "lambda [nm]"
set yl "I_lambda [] (shifted by 1/4 dataset number)"

set ytics shift
set mytics 1
set grid ytics mytics
set zeroaxis
set bar 0.5

p \
  "synthetic.dat" u ($2/nm):($3+shift*$4)   t "synthetic" w l lt 7,\
  "Spectra.dat"   u ($2/nm):($3+shift*$5):4 t "observed" w err lt 3 pt 1 ps 0,\
  "chi2_SYN.dat"  u ($2/nm):($3+shift*$5)   t "residua"  w l lt 1 lw 1
pa -1

set term png small
set out "chi2_SYN.png"
rep

q

  "<awk '($NF+0>100)' chi2_VIS.dat" u (sqrt($2**2+$3**2)/$4):($5+$7) t "chi^2 > 100" w p lt 1 pt 6 ps 1.5

