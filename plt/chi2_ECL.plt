#!/usr/bin/env gnuplot

set colors classic
#set term x11

set xl "JD - 2400000"
set yl "eclipse duration [day]"

set yr [0:]
set zeroaxis
set key bottom

load "T0.plt"
set arrow from T0-2400000,graph 0 to T0-2400000,graph 1 nohead lt 0

p "duration.dat" u ($1-2400000):2   t "1+2" w lp pt 1 lc 'gray',\
  "Ecl12.dat"    u ($1-2400000):2:3 t "observ." w err lt 3 pt 1 ps 0.5,\
  "chi2_ECL.dat" u ($1-2400000):2:3 t "residua" w l lc 'red' lw 3,\
  "Omc12.dat"    u ($1-2400000):(0.0) t "minima" w p pt 2 lc 'green'

pa -1

set term png small
set out "chi2_ECL.png"
rep



