#!/usr/bin/gnuplot

set colors classic
set term post eps enh color dashed
set out "chi2_ECL.eps"
set size 1.0,0.7

set xl "JD {/Symbol -} 2400000"
set yl "eclipse duration [day]" offset +0.5,0

y1=0.24
set yr [y1:]
set zeroaxis
set key right font "Helvetica,14"

load "T0.plt"
set arrow from T0-2400000,graph 0 to T0-2400000,graph 1 nohead lt 0

p \
  "duration.dat" u ($1-2400000):2   t "synthetic duration" w lp pt 1 lc 'gray',\
  "Ecl12.dat"    u ($1-2400000):2:3 t "observed duration" w err lc "blue" pt 1 ps 0.5,\
  "chi2_ECL.dat" u ($1-2400000):2:3 t "residua" w l lc 'red' lw 3,\
  "Omc12.dat"    u ($1-2400000):(y1) t "minima" w p pt 2 ps 2 lc 'green',\

q


