#!/usr/bin/gnuplot

set term x11

set xl "iter"
set yl "chi^2"

set logscale y
set mytics 10
set yr [1e0:1e8]
set ytics 10
set mytics 10

p "<awk '/# chi/{ i++; print i, $NF; }' simann.out"     u 1:2 t "chi^2" w l,\
  "<awk '/# chi/{ i++; print i, $(NF-11); }' simann.out" u 1:2 t "SKY"   w l,\
  "<awk '/# chi/{ i++; print i, $(NF-10); }' simann.out" u 1:2 t "TTV"   w l,\
  "<awk '/# chi/{ i++; print i, $(NF -9); }' simann.out" u 1:2 t "RV"    w l,\
  "<awk '/# chi/{ i++; print i, $(NF -8); }' simann.out" u 1:2 t "ECL"   w l,\
  "<awk '/# chi/{ i++; print i, $(NF -7); }' simann.out" u 1:2 t "VIS"   w l,\
  "<awk '/# chi/{ i++; print i, $(NF -6); }' simann.out" u 1:2 t "CLO"   w l,\
  "<awk '/# chi/{ i++; print i, $(NF -5); }' simann.out" u 1:2 t "T3"    w l,\
  "<awk '/# chi/{ i++; print i, $(NF -4); }' simann.out" u 1:2 t "LC"    w l,\
  "<awk '/# chi/{ i++; print i, $(NF -3); }' simann.out" u 1:2 t "SYN"   w l,\
  "<awk '/# chi/{ i++; print i, $(NF -2); }' simann.out" u 1:2 t "SED"   w l,\
  "<awk '/# chi/{ i++; print i, $(NF -1); }' simann.out" u 1:2 t "MASS"  w l
pa -1

set term png small
set out "chi2_iter.png"
rep


