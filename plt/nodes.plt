#!/usr/bin/gnuplot

set xl "x [arcsec]"
set yl "y [arcsec]"

tmp=0.25
dx=tmp
dy=tmp

set xr [-0.75*tmp:3.75*tmp]
set yr [-0.75*tmp:0.75*tmp]

set xtics 0.5*tmp
set ytics 0.5*tmp
set grid xtics ytics
set size ratio -1
set nokey

PHASESHIFT = -0.05
PHASESHIFT = 0.0

frc(x) = x-int(x)
mod(x,y) = frc(x/y)*y
f(x) = mod(x-1,4)*dx
g(x) = int((x-1)/4)*dy

p \
  "<awk '(FNR>1){ print $0,ARGIND; }' nodes*.dat"  u ($2+f($5)):($3+g($5)) w d lt 1,\
  "<awk '(FNR==1){ print s; }(FNR>1){ print $0,ARGIND; }' nodes*.silh" u ($1+f($3)):($2+g($3)) w l lc 'orange',\
  "<tail nodes*.dat | awk '/# phase = /{ i++; print $4,i; }'" u (f($2)):(g($2)+0.5*tmp):(sprintf("%.2f", $1+PHASESHIFT)) w labels,\

pa -1

set term png small
set out "nodes.png"
rep

q


