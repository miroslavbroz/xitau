#!/usr/bin/gnuplot

set term post eps enh color solid "Helvetica" 18
set out "chi2_AO.eps"
set size 0.9,0.98

set colors classic

set xl "{/Helvetica-Oblique u} [arcsec]"
set yl "{/Helvetica-Oblique v} [arcsec]" offset +1,0

tmp=0.25
dx=tmp
dy=tmp

set xr [-0.75*tmp:3.75*tmp]
set yr [-0.75*tmp:2.75*tmp]

set xtics tmp
set ytics tmp
set grid xtics ytics lc 'black' front
set size ratio -1
set key samplen 1.0

frc(x) = x-int(x)
mod(x,y) = frc(x/y)*y
f(x) = mod(x-1,4)*dx
g(x) = int((x-1)/4)*dy

set lmargin 8.5
set rmargin 0.2
set bmargin 3.1
set tmargin 0.2

p \
  "../chi2_AO.dat" u ($2+f($6)):($3+g($6)) t "residua" w l lc 'red',\
  "<awk '(NF>0) && ($6!=l){ print; l=$6; }' ../chi2_AO.dat" u (f($6)):(g($6)-0.125):(sprintf("%.2f", $1-2400000.0)) not w labels center font "Helvetica,14",\
  "<awk '(FNR>1){ print $0,ARGIND; }' ../nodes*.dat"  u ($2+f($5)):($3+g($5)) not w d lc 'gray',\
  "<awk '(FNR==1){ print s; }(FNR>1){ print $0,ARGIND; }' ../nodes*.silh"  u ($1+f($3)):($2+g($3)) t "synthetic" w l lc 'orange',\
  "<awk '(FNR==1){ print s; }(FNR>1){ print $0,ARGIND; }' ../nodes*.silh_" u ($1+f($3)):($2+g($3)) t "observed"  w l lc 'blue',\

q


