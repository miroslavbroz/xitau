#!/usr/bin/gnuplot

JD0 = 2449425.099
P = 5.998693

f1(x) = x > 0.0 ? x : x+1.0
frac(x) = f1(x-int(x))
phase(jd) = frac((jd-JD0)/P)

AU = 1.49597870691e11  # m
day = 86400.           # s

f(RV) = RV*1.e3 * 86400./AU
g(vb_z) = vb_z*AU/86400. / 1.e3

########################################################################

set colors classic
set term x11

set xl "phase"
set yl "{/Helvetica-Oblique v}_{rad} [km/s]" offset +1,0

set yr [-400:400]
set zeroaxis

load "T0.plt"
set arrow from T0-2400000,graph 0 to T0-2400000,graph 1 nohead lt 0

p \
  "<awk '($2==-1)' out_JDATE_barycentric.dat" u (phase($1)):(g($8)) t "Ac1" w d lt 1,\
  "<awk '($2==-2)' out_JDATE_barycentric.dat" u (phase($1)):(g($8)) t "Ac2" w d lt 2,\
  "RV1.dat" u (phase($1)):2:3 t "observ." w err lt 1 pt 1 ps 0.5,\
  "RV2.dat" u (phase($1)):2:3 not         w err lt 2 pt 1 ps 0.5,\
  "<awk '($4==1) || (NF==0)' chi2_RV.dat" u (phase($1)):2 t "residua" w l lt 1 lw 3,\
  "<awk '($4==2) || (NF==0)' chi2_RV.dat" u (phase($1)):2 not         w l lt 1 lw 3

pa -1

set term post eps enh color dashed
set out "chi2_RV1.eps"
set size 1.0,0.7
rep

q


