#!/usr/bin/gnuplot

set term post eps enh color solid
set out "orbit1.eps"

set origin 0,0
set size 0.6,1.38
set multiplot
set size 0.6,0.33
set origin 0,0
dy=0.267

set xl "JD - 2400000"
set yl "{/Helvetica-Oblique a}_1 [AU]" offset 1.5,0
set xr [45000:57500]
set lmargin 9.2
set rmargin 0.1

load "config.plt"
load "T0.plt"

set arrow from T0-2400000,graph 0 to T0-2400000,graph 1 nohead lt 0

p "<awk '($2==-2)' xvpl2el.out" u ($1-2400000):3 not w l ls 2,\

set origin 0,1*dy
set xl ""
set yl "{/Helvetica-Oblique e}_1 []" offset 1,0
set xtics (\
  "" 46000 0,\
  "" 48000 0,\
  "" 50000 0,\
  "" 52000 0,\
  "" 54000 0,\
  "" 56000 0,\
  )

p "<awk '($2==-2)' xvpl2el.out" u ($1-2400000):4 not w l ls 2,\

set origin 0,2*dy
set yl "{/Helvetica-Oblique i}_1 [deg]"

p "<awk '($2==-2)' xvpl2el.out" u ($1-2400000):5 not w l ls 2

set origin 0,3*dy
set yl "{/Symbol W}_1 [deg]"

p "<awk '($2==-2)' xvpl2el.out" u ($1-2400000):6 not w d ls 2

set origin 0,4*dy
set yl "{/Symbol w}_1 [deg]"
set yr [0:360]

p "<awk '($2==-2)' xvpl2el.out" u ($1-2400000):7 not w d ls 2


