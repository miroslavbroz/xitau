#!/usr/bin/gnuplot

set colors classic
set term x11

rad = 180./pi

set xl "B/lambda [cycles]"
set yl "closure phase arg T_3 [deg] (shifted by dataset number)"

set ytics 360
set mytics 2
set grid ytics mytics
set zeroaxis
set colorbox
set palette rgbformulae 33,13,10
set cbr [0:100]

load "T0.plt"
set arrow from T0-2400000,graph 0 to T0-2400000,graph 1 nohead lt 0

p \
  "Clo.dat"          u (sqrt($2**2+$3**2)/$6):($10+($12-7)*360*1):11 t "observed closure phase" w err lt 3 pt 1 ps 0.5,\
  "closurephase.dat" u (sqrt($2**2+$3**2)/$6):($8+($9-7)*360*1)  t "synthetic closure phase" w p lt 7 pt 1,\
  "chi2_CLO.dat"     u (sqrt($2**2+$3**2)/$6):($10+($12-7)*360*1):13 t "residua" w l lt 1 lw 3 lc palette z,\
  "<awk '($NF+0>100)' chi2_CLO.dat" u (sqrt($2**2+$3**2)/$6):($10+($12-7)*360*1) t "chi^2 > 100" w p lt 1 pt 6 ps 1.5,\

pa -1

set term png small size 2048,1024
set out "chi2_CLO.png"
rep



