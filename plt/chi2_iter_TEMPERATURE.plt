#!/usr/bin/gnuplot

set term x11

set xl "iter"
set yl "temperature"

set logscale y
set mytics 10
#set yr [0.1:]
set grid

p "<awk 'BEGIN{ itertot = 0; }/# itertot/{ itertot = $4; }/# temptr/{ print itertot, $4; }' simann.out"  u 1:2 not w lp

pa -1

set term png small
set out "chi2_iter_TEMPERATURE.png"
rep


