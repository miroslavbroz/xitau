#!/usr/bin/gnuplot

set term x11

set xl "iter"
set yl "temperature"

set xr [0:10000]
set logscale y
set mytics 10
#set yr [0.1:]
set grid
set samples 1000

nparam = 0
temptr = 200000.
iter_at_temp = 50
eps_temptr = 0.10

p "<awk 'BEGIN{ itertot = 0; }/# itertot/{ itertot = $4; }/# temptr/{ print itertot, $4; }' simann.out"  u 1:2 not w lp,\
  (1.-eps_temptr)**(int((x-nparam)/iter_at_temp))*temptr

pa -1

set term png small
set out "chi2_iter_TEMPERATURE.png"
rep


