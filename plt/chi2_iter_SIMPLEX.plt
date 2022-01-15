#!/usr/bin/gnuplot

chi2 = `awk '/# chi/{ chi=$NF; }END{ print chi; }' simplex.out`

set colors classic
set term x11

set xl "iter"
set yl "chi^2"

set logscale y
set mytics 10
set yr [1e0:1e8]
set ytics 10
set mytics 10

nfree = 13
set arrow from nfree,graph 1 to nfree,graph 0 lt 0 nohead

p "<awk '/# chi/{ i++; print i, $NF; }' simplex.out"      u 1:2 t "chi^2" w lp lt 1 ps 0.5,\
  "<awk '/# chi/{ i++; print i, $(NF-12); }' simplex.out" u 1:2 t "SKY"   w l,\
  "<awk '/# chi/{ i++; print i, $(NF-11); }' simplex.out" u 1:2 t "RV"    w l,\
  "<awk '/# chi/{ i++; print i, $(NF-10); }' simplex.out" u 1:2 t "TTV"   w l,\
  "<awk '/# chi/{ i++; print i, $(NF -9); }' simplex.out" u 1:2 t "ECL"   w l,\
  "<awk '/# chi/{ i++; print i, $(NF -8); }' simplex.out" u 1:2 t "VIS"   w l,\
  "<awk '/# chi/{ i++; print i, $(NF -7); }' simplex.out" u 1:2 t "CLO"   w l,\
  "<awk '/# chi/{ i++; print i, $(NF -6); }' simplex.out" u 1:2 t "T3"    w l,\
  "<awk '/# chi/{ i++; print i, $(NF -5); }' simplex.out" u 1:2 t "LC"    w l,\
  "<awk '/# chi/{ i++; print i, $(NF -4); }' simplex.out" u 1:2 t "SYN"   w l,\
  "<awk '/# chi/{ i++; print i, $(NF -3); }' simplex.out" u 1:2 t "SED"   w l,\
  "<awk '/# chi/{ i++; print i, $(NF -2); }' simplex.out" u 1:2 t "AO"    w l,\
  "<awk '/# chi/{ i++; print i, $(NF -1); }' simplex.out" u 1:2 t "MASS"  w l,\
  chi2 w l lt 0
pa -1

set term png small
set out "chi2_iter.png"
rep


