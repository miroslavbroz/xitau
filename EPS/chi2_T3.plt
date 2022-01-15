#!/usr/bin/gnuplot

set colors classic
set term post eps enh color dashed
set out "chi2_T3.eps"
set size 1.0,0.9

rad = 180./pi

set xl "{/Helvetica-Oblique B}/{/Symbol l} [cycles]"
set yl "|{/Helvetica-Oblique T}_3| [] (shifted by dataset number)"

set yr [:7]
#set ytics 1
set grid ytics ytics
set zeroaxis

load "T0.plt"
set arrow from T0-2400000,graph 0 to T0-2400000,graph 1 nohead lt 0

fac=2.0

p \
  "Clo.dat"          u (sqrt($2**2+$3**2)/$6):($8+$12*fac-fac):9 t "observed" w err lt 3 pt 1 ps 0.5,\
  "closurephase.dat" u (sqrt($2**2+$3**2)/$6):($7+$9*fac-fac)    t "synthetic" w p lc 'orange' pt 1,\
  "chi2_T3.dat"      u (sqrt($2**2+$3**2)/$6):($8+$12*fac-fac)   t "residua" w l lt 1 lw 1

q


