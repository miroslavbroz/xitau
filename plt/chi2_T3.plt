#!/usr/bin/gnuplot

set term x11

rad = 180./pi

set xl "B/lambda [cycles]"
set yl "triple product amplitude |T_3| [] (shifted by dataset number)"

set yr [:65]
#set ytics 1
set grid ytics ytics
set zeroaxis

load "T0.plt"
set arrow from T0-2400000,graph 0 to T0-2400000,graph 1 nohead lt 0

p \
  "closurephase.dat" u (sqrt($2**2+$3**2)/$6):($7+$9*2)    t "synthetic T3 amplitude" w p lt 7 pt 1,\
  "Closure.dat"      u (sqrt($2**2+$3**2)/$6):($8+$12*2):9 t "observed T3 amplitude" w err lt 3 pt 1 ps 0.5,\
  "chi2_T3.dat"      u (sqrt($2**2+$3**2)/$6):($7+$11*2)   t "residua" w l lt 1 lw 1
pa -1

set term png small
set out "chi2_T3.png"
rep



