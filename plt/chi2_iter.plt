#!/usr/bin/gnuplot

chi2 = `awk 'BEGIN{ min=1e38; }{ chi=$NF; if (chi<min){ min=chi; } }END{ print min; }' chi2_func.tmp`
nfree = 47

set colors classic
set term x11

set xl "iter"
set yl "chi^2"

set yr [1e0:1e8]
#set yr [6000:8000]
set logscale y
set ytics 10
set mytics 10
set key outside samplen 1.5

set arrow from nfree,graph 1 to nfree,graph 0 lt 0 nohead
set label sprintf(" %.1f ", chi2) at graph 1,first chi2*1.5 right

p "<awk '{ i++; print i, $NF; }' chi2_func.tmp"      u 1:2 t "chi^2" w lp lt 1 ps 0.8,\
  "<awk '{ i++; print i, $(NF-15); }' chi2_func.tmp" u 1:2 t "SKY"   w l,\
  "<awk '{ i++; print i, $(NF-14); }' chi2_func.tmp" u 1:2 t "RV"    w l,\
  "<awk '{ i++; print i, $(NF-13); }' chi2_func.tmp" u 1:2 t "TTV"   w l,\
  "<awk '{ i++; print i, $(NF-12); }' chi2_func.tmp" u 1:2 t "ECL"   w l,\
  "<awk '{ i++; print i, $(NF-11); }' chi2_func.tmp" u 1:2 t "VIS"   w l,\
  "<awk '{ i++; print i, $(NF-10); }' chi2_func.tmp" u 1:2 t "CLO"   w l,\
  "<awk '{ i++; print i, $(NF -9); }' chi2_func.tmp" u 1:2 t "T3"    w l dt 2,\
  "<awk '{ i++; print i, $(NF -8); }' chi2_func.tmp" u 1:2 t "LC"    w lp lc 'orange' pt 2 ps 0.8,\
  "<awk '{ i++; print i, $(NF -7); }' chi2_func.tmp" u 1:2 t "SYN"   w l lw 2,\
  "<awk '{ i++; print i, $(NF -6); }' chi2_func.tmp" u 1:2 t "SED"   w l lw 2,\
  "<awk '{ i++; print i, $(NF -5); }' chi2_func.tmp" u 1:2 t "AO"    w l,\
  "<awk '{ i++; print i, $(NF -4); }' chi2_func.tmp" u 1:2 t "SKY2"  w l,\
  "<awk '{ i++; print i, $(NF -3); }' chi2_func.tmp" u 1:2 t "SKY3"  w l,\
  "<awk '{ i++; print i, $(NF -2); }' chi2_func.tmp" u 1:2 t "OCC"   w l,\
  "<awk '{ i++; print i, $(NF -1); }' chi2_func.tmp" u 1:2 t "MASS"  w l dt 4,\
  chi2 w l lt 0 not

pa -1

set term png small
set out "chi2_iter.png"
rep

q

  43.06 w l lt 0


