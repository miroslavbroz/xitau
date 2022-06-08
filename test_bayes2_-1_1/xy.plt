#!/usr/bin/gnuplot

f(x) = a*x+b

fit f(x) "xy.dat" u 1:2 via a,b



