#!/usr/bin/gnuplot

set colors classic
set term post eps enh color dashed
set out "chi2_CLO.eps"
set size 1.0,0.9

rad = 180./pi

set xl "{/Helvetica-Oblique B}/{/Symbol l} [cycles]"
set yl "arg {/Helvetica-Oblique T}_3 [deg] (shifted by dataset number)"

set ytics 360
set mytics 2
set grid ytics mytics
set zeroaxis

load "T0.plt"
set arrow from T0-2400000,graph 0 to T0-2400000,graph 1 nohead lt 0

p \
  "Clo.dat"          u (sqrt($2**2+$3**2)/$6):($10+$12*360):11 t "observed" w err lt 3 pt 1 ps 0.5,\
  "closurephase.dat" u (sqrt($2**2+$3**2)/$6):($8+$9*360)  t "synthetic" w p lc 'orange' pt 1,\
  "chi2_CLO.dat"     u (sqrt($2**2+$3**2)/$6):($10+$12*360) t "residua" w l lt 1 lw 1,\
  "<awk '($NF+0>100)' chi2_CLO.dat" u (sqrt($2**2+$3**2)/$6):($10+$12*360) t "{/Symbol c}^2 > 100" w p lt 1 pt 6 ps 1.5,\

q

