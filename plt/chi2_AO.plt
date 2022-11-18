#!/usr/bin/gnuplot

km = 1.e3
au = 1.49597870700e11  # m, from IAU 2012
deg = pi/180.
arcsec = deg/3600.

set colors classic
set term x11

set xl "u [arcsec]"
set yl "v [arcsec]"

tmp=0.25
dx=tmp
dy=tmp

i=5
j=7.5
set xr [-0.75*tmp:(i-1+0.75)*tmp]
set yr [-0.75*tmp:(j-1+0.75)*tmp]

set xtics 0.5*tmp
set ytics 0.5*tmp
set grid xtics ytics
set size ratio -1
set key samplen 1.0 outside

frc(x) = x-int(x)
mod(x,y) = frc(x/y)*y
f(x) = mod(x-1,i)*dx
g(x) = int((x-1)/i)*dy

h(x,d) = x*100.*km/(d*au)/arcsec

p \
  "chi2_AO.dat" u ($2+f($6)):($3+g($6)) t "residua" w l lc 'red',\
  "<awk '(FNR>1){ print $0,ARGIND; }' nodes*.dat"  u ($2+f($5)):($3+g($5)) t "shape" w d lc 'gray',\
  "<awk '(FNR==1){ print s; }(FNR>1){ print $0,ARGIND; }' nodes*.silh"  u ($1+f($3)):($2+g($3)) t "synthetic" w l lc 'orange',\
  "<awk '(FNR==1){ print s; }(FNR>1){ print $0,ARGIND; }' nodes*.silh_" u ($1+f($3)):($2+g($3)) t "observed"  w l lc 'blue',\
  "<awk '($4<=0) || (NF==0)' poles.dat"  u (h($2,$5)+f($1)):(h($3,$5)+g($1)) t "poles" w lines lc 'green' lw 3,\
  "<awk '($4>=0) || (NF==0)' poles.dat"  u (h($2,$5)+f($1)):(h($3,$5)+g($1)) not       w lines lc 'cyan'  lw 3,\

pa -1

set term png small size 2048,2048
set out "chi2_AO.png"
rep

q


  "<awk '(NF==0){ i=0; }!/^#/ && (NF>0){ i++; }(i==2){ print; }' chi2_AO.dat" u ($2+f($5)):($3+g($5)):4:4 not w xyerr lc 'blue',\


