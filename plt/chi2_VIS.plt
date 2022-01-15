#!/usr/bin/gnuplot

set colors classic
set term x11

set xl "B/lambda [cycles]"
set yl "V^2 [] (shifted by dataset number)"

set ytics 5
set mytics 5
set grid ytics mytics
set zeroaxis

fac=2.0

p \
  "chi2_VIS.dat"   u (sqrt($2**2+$3**2)/$4):($6+fac*$8)   t "residua" w l lt 1 lw 1,\
  "Vis.dat"        u (sqrt($2**2+$3**2)/$4):($6+fac*$8):7 t "observed visibility" w err lt 3 pt 1 ps 0.5,\
  "visibility.dat" u (sqrt($2**2+$3**2)/$4):($5+fac*$6)   t "synthetic visibility" w p lt 7 pt 1,\

pa -1

set term png small size 2048,1024
set out "chi2_VIS.png"
rep

q

  "<awk '($NF+0>100)' chi2_VIS.dat" u (sqrt($2**2+$3**2)/$4):($6+$8) t "chi^2 > 100" w p lt 1 pt 6 ps 1.5


