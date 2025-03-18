#!/usr/bin/gnuplot

logM(X) = (((-1.744951e0*X + 30.31681e0)*X - 196.2387e0)*X + 562.6774e0)*X - 604.0760e0
logR(X) = (((-0.8656627e0*X + 16.22018e0)*X - 112.2303e0)*X + 341.6602e0)*X - 387.0969e0
Mbol(X) = (((4.328314e0*X - 81.10091e0)*X + 561.1516e0)*X - 1718.301e0)*X + 1977.795e0

set xr [4.6:3.4]
set samples 100

set xl "logT"
set yl "logM"

set arrow from log10(3500.),graph 0 rto 0,graph 1 nohead lt 0 front
set arrow from graph 0,first log10(0.33) rto graph 1,0 nohead lt 0 front

p logM(x)

pa -1

set term png small
set out "hec88.png"
rep

