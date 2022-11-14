#!/usr/bin/gnuplot

set term x11

set xl "face"
set yl "surf [m^2]"

set zeroaxis

set arrow from 53,graph 0 rto 0,graph 1 nohead lt 0
#set arrow from 79,graph 0 rto 0,graph 1 nohead lt 0

p \
  "output.surf.01" u 1:2 w lp,\
  "output.surf.49" u 1:2 w lp,\
  "output.surf.50" u 1:2 w lp,\

pa -1

q


