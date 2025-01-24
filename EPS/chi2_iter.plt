#!/usr/bin/gnuplot

chi2 = `awk 'BEGIN{ min=1e38; }{ chi=$NF; if (chi<min){ min=chi; } }END{ print min; }' chi2_func.tmp`
nfree = 13

set colors classic
set term x11

set xl "{/Helvetica-Oblique N}_{iter}"
set yl "{/Symbol c}^2" offset -0.75,0

set xr [0:]
set yr [1e2:1e7]
set logscale y
set ytics 10 format "10^{%T}"
set mytics 10
set key outside spacing 1.3 samplen 1.5 width -2 font "Helvetica,12"

set arrow from nfree,graph 1 to nfree,graph 0 lt 0 nohead
#set label sprintf(" %.1f ", chi2) at graph 1,first chi2*1.5 right

set lmargin 6.5
set rmargin 8.0
set bmargin 3.1
set tmargin 0.7

p "<awk '{ i++; print i, $NF; }' chi2_func.tmp"      u 1:2 t "total" w lp pt 1 ps 0.5 lc 'red',\
  "<awk '{ i++; print i, $(NF-13); }' chi2_func.tmp" u 1:2 t "RV"    w l lc 'blue',\
  "<awk '{ i++; print i, $(NF-12); }' chi2_func.tmp" u 1:2 t "TTV"   w l lc 'magenta',\
  "<awk '{ i++; print i, $(NF-10); }' chi2_func.tmp" u 1:2 t "VIS"   w l lc '#bb0000',\
  "<awk '{ i++; print i, $(NF -9); }' chi2_func.tmp" u 1:2 t "CLO"   w l lc 'orange',\
  "<awk '{ i++; print i, $(NF -8); }' chi2_func.tmp" u 1:2 t "T3"    w l lc '#dddd00',\
  "<awk '{ i++; print i, $(NF -6); }' chi2_func.tmp" u 1:2 t "SYN"   w l lw 3 lc 'green',\
  "<awk '{ i++; print i, $(NF -5); }' chi2_func.tmp" u 1:2 t "SED"   w l lw 3 lc 'blue',\
  "<awk '{ i++; print i, $(NF -3); }' chi2_func.tmp" u 1:2 t "SKY2"  w l lc 'cyan',\
  chi2 w l lt 0 not

#pa -1

set term post eps enh color solid "Helvetica,18"
set out "chi2_iter.eps"
set size 0.8,0.8
rep

q


  "<awk '{ i++; print i, $(NF-14); }' chi2_func.tmp" u 1:2 t "SKY"   w l,\
  "<awk '{ i++; print i, $(NF-11); }' chi2_func.tmp" u 1:2 t "ECL"   w l,\
  "<awk '{ i++; print i, $(NF -7); }' chi2_func.tmp" u 1:2 t "LC"    w l,\
  "<awk '{ i++; print i, $(NF -4); }' chi2_func.tmp" u 1:2 t "AO"    w l,\
  "<awk '{ i++; print i, $(NF -2); }' chi2_func.tmp" u 1:2 t "SKY3"  w l,\
  "<awk '{ i++; print i, $(NF -1); }' chi2_func.tmp" u 1:2 t "MASS"  w l dt 4,\


