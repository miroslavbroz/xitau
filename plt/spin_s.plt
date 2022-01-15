#!/usr/bin/gnuplot

load "T0.plt"

set xl "JD - 2400000"
set yl "sx, sy, sz []"

set arrow from T0-2400000,graph 0 rto 0,graph 1 nohead lt 0

p \
  "<awk '($1==-1)' spin.out" u ($2-2400000):($3-0.27598089557780503) w lp,\
  "<awk '($1==-1)' spin.out" u ($2-2400000):($4-0.90055038986246250) w lp,\
  "<awk '($1==-1)' spin.out" u ($2-2400000):($5-0.33592192633801032) w lp

pa -1


