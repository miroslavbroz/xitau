#!/usr/bin/gnuplot

set colors classic
set term x11

AU = 1.49597870691e11  # m
day = 86400.           # s

f(RV) = RV*1.e3 * 86400./AU
g(vb_z) = vb_z*AU/86400. / 1.e3

jd(bessel) = (bessel-1900.0)*365.242198781 + 2415020.31352;

########################################################################

set xl "JD - 2400000"
set yl "vzb [km/s]"

#tmp=60.0; set yr [-tmp:tmp]
set zeroaxis

load "T0.plt"
set arrow from T0-2400000,graph 0 to T0-2400000,graph 1 nohead lt 0

gamma = x_param21
set arrow from graph 0,first gamma rto graph 1,first 0 nohead lt 0
#set arrow from jd(1960.0941692)-2400000,graph 0 rto 0,graph 1 nohead lt 0

p \
  "<awk '($2==-1)' out_JDATE_barycentric.dat" u ($1-2400000):(g($8)) t "1" w l lt 1,\
  "<awk '($2==-2)' out_JDATE_barycentric.dat" u ($1-2400000):(g($8)) t "2" w l lt 2,\
  "<awk '($2==-3)' out_JDATE_barycentric.dat" u ($1-2400000):(g($8)) t "3" w l lt 3,\
  "<awk '($2==-4)' out_JDATE_barycentric.dat" u ($1-2400000):(g($8)) t "4" w l lt 4,\
  "RV1.dat" u ($1-2400000):2:3 t "observ." w err lt 1 pt 1 ps 0.5,\
  "RV2.dat" u ($1-2400000):2:3 not         w err lt 2 pt 1 ps 0.5,\
  "RV3.dat" u ($1-2400000):2:3 not         w err lt 3 pt 1 ps 0.5,\
  "<awk '($4==1) || (NF==0)' chi2_RV.dat" u ($1-2400000):2 t "residua" w l lt 1 lw 3,\
  "<awk '($4==2) || (NF==0)' chi2_RV.dat" u ($1-2400000):2 not         w l lt 1 lw 3,\
  "<awk '($4==3) || (NF==0)' chi2_RV.dat" u ($1-2400000):2 not         w l lt 1 lw 3,\
  "<awk '($NF+0>100)' chi2_RV.dat" u ($1-2400000):2 t "chi^2 > 100" w p lt 1 pt 6 ps 1.5,\
  "RV1.dat_GAMMA12_rejected" u ($1-2400000):2 t "systematics?" w p pt 2 ps 2 lc 'black'

pa -1

set term png small
set out "chi2_RV.png"
rep

q


