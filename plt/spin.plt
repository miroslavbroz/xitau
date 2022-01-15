#!/usr/bin/gnuplot

load "T0.plt"

set xl "JD - 2400000"
set yl "omega [rad d^-1]"

set arrow from T0-2400000,graph 0 rto 0,graph 1 nohead lt 0

p "<awk '($1==-1)' spin.out" u ($2-2400000):6 w lp

pa -1


