#!/usr/bin/gnuplot

f(x,x1,x2) = ( (x - (x1+x2)/2.) * 2./(x2-x1) )**100
g(x,x1,x2) = x < x1 ? 1.0e6 - 1.0e6*(x-x1)/(x2-x1) : \
  x > x2 ? 1.0e6 + 1.0e6*(x-x2)/(x2-x1) : \
  0.0

set xr [0:10]
set yr [0:2e6]
#set logscale y
set samples 1000

set arrow from 0.5,graph 0 rto 0,graph 1 nohead lt 0 front
set arrow from 0.33,graph 0 rto 0,graph 1 nohead lt 0 front

p \
  f(x, 0.7, 10.0-0.7) w lp,\
  g(x, 0.5, 10.0-0.5) w lp,\

pa -1

set term png small
set out "constraint.png"
rep


