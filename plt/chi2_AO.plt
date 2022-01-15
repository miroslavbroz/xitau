#!/usr/bin/gnuplot

set colors classic
set term x11

set xl "u [arcsec]"
set yl "v [arcsec]"

tmp=0.25
dx=tmp
dy=tmp

set xr [-0.75*tmp:4.75*tmp]
set yr [-0.75*tmp:6.75*tmp]

set xtics 0.5*tmp
set ytics 0.5*tmp
set grid xtics ytics
set size ratio -1
set key samplen 1.0

frc(x) = x-int(x)
mod(x,y) = frc(x/y)*y
f(x) = mod(x-1,5)*dx
g(x) = int((x-1)/5)*dy

p \
  "chi2_AO.dat" u ($2+f($6)):($3+g($6)) t "residua" w l lc 'red',\
  "<awk '(FNR>1){ print $0,ARGIND; }' nodes*.dat"  u ($2+f($5)):($3+g($5)) t "shape" w d lc 'gray',\
  "<awk '(FNR==1){ print s; }(FNR>1){ print $0,ARGIND; }' nodes*.silh"  u ($1+f($3)):($2+g($3)) t "synthetic" w l lc 'orange',\
  "<awk '(FNR==1){ print s; }(FNR>1){ print $0,ARGIND; }' nodes*.silh_" u ($1+f($3)):($2+g($3)) t "observed"  w l lc 'blue',\

pa -1

set term png small size 2048,2048
set out "chi2_AO.png"
rep

q


  "<awk '(NF==0){ i=0; }!/^#/ && (NF>0){ i++; }(i==2){ print; }' chi2_AO.dat" u ($2+f($5)):($3+g($5)):4:4 not w xyerr lc 'blue',\


