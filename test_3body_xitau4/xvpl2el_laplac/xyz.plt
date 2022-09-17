#!/usr/bin/gnuplot

set colors classic

set xl "x [au]"
set yl "y [au]"
set zl "z [au]"

tmp=40
tmp=5
set xr [-tmp:tmp]
set yr [-tmp:tmp]
set zr [-tmp:tmp]
set view equal xyz
set xyplane 0.0
set zeroaxis
set view 90,0
set nokey
scl = 100
scl = 10

sp "<awk '($2==-1)' out_JDATE_barycentric.dat" u 3:4:5 w d,\
   "<awk '($2==-2)' out_JDATE_barycentric.dat" u 3:4:5 w d,\
   "<awk '($2==-3)' out_JDATE_barycentric.dat" u 3:4:5 w d,\
   "<awk '(NR%100==1){ print 0,0,0,0; print $0; print s; print s; }' angmom.tmp" u (scl*$2):(scl*$3):(scl*$4) w p

pa -1

q

   "<awk '($2==-4)' out_JDATE_barycentric.dat" u 3:4:5 w d,\

