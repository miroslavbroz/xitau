#!/usr/bin/gnuplot

set xl "lambda [deg]"
set yl "phi [deg]"

set xr [-180:180]
set yr [-90:90]
set xtics 30
set ytics 30
set grid
set size ratio -1
set zeroaxis

p "world_50m.txt" u 1:2 w l lc 'black'

pa -1

