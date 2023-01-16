#!/usr/bin/gnuplot

w = 256
h = 256

set colors classic
set term x11

set xl "u [pxl]"
set yl "v [pxl]"
set cbl "chi^2 [1]"

tmp=50
set xtics 0.5*tmp
set ytics 0.5*tmp
set grid xtics ytics
set cbr [1.0:]
set size ratio -1
set logscale cb
set palette rgbformulae 33,13,10

dx=tmp
dy=tmp
i=5
j=7.0

frc(x) = x-int(x)
mod(x,y) = frc(x/y)*y
f(x) = mod(x-1,i)*dx
g(x) = int((x-1)/i)*dy

p \
  "chi2_AO2.dat" u ($3-0.5*w+f($1)):(-$2+0.5*h+g($1)):7 w p pt 5 lc palette z not

pa -1

set term png small size 1024,1024
set out "chi2_AO2.png"
rep

q


