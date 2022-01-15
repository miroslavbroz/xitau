#!/usr/bin/gnuplot

AU = 1.49597870691e11  # m
day = 86400.           # s

f(RV) = RV*1.e3 * 86400./AU
g(vb_z) = vb_z*AU/86400. / 1.e3

########################################################################

set term post eps enh color dashed
set out "chi2_RV.eps"
set size 1.0,0.7

set xl "JD {/Symbol -} 2400000"
set yl "{/Helvetica-Oblique v}_{rad} [km/s]" offset +1,0

set zeroaxis
#set xr [48000:58500]
set xtics 1000
y2=150
y1=-y2
#set yr [y1:y2]
set key samplen 1.0
set grid noxtics noytics front

load "config.plt"

set bmargin 3.2
set tmargin 1.25
set lmargin 7.0
set rmargin 0.1

load "T0.plt"
set label sprintf("{/=10 {/Helvetica-Oblique T}_0 = %.6f}", T0-2400000) at T0-2400000-100, graph 1.04

p \
  "<awk '($2==-1)' out_JDATE_barycentric.dat" u ($1-2400000):(g($8)) t "Aa" w l ls 1,\
  "<awk '($2==-2)' out_JDATE_barycentric.dat" u ($1-2400000):(g($8)) t "Ab" w l ls 2,\
  "<awk '($2==-3)' out_JDATE_barycentric.dat" u ($1-2400000):(g($8)) t "B" w l ls 3,\
  "<awk '($2==-4)' out_JDATE_barycentric.dat" u ($1-2400000):(g($8)) t "C" w l ls 4,\
  "RV1.dat" u ($1-2400000):2:3 t "{/=10 observ.}" w err ls 5,\
  "RV2.dat" u ($1-2400000):2:3 not                w err ls 6,\
  "RV3.dat" u ($1-2400000):2:3 not                w err ls 7,\
  "<awk '($4==1) || (NF==0)' chi2_RV.dat" u ($1-2400000):(g($2)) t "{/=10 residua}" w l ls 9,\
  "<awk '($4==2) || (NF==0)' chi2_RV.dat" u ($1-2400000):(g($2)) not                w l ls 9,\
  "<awk '($4==3) || (NF==0)' chi2_RV.dat" u ($1-2400000):(g($2)) not                w l ls 9,\
  "vline.dat" u (T0-2400000):(y1+(y2-y1)*$2) not w l lt 0,\
#pa -1

q


