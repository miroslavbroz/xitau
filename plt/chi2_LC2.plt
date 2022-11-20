#!/usr/bin/gnuplot

set colors classic
set term x11

band = 7
shift = 0.0

set xl "JD - 2400000"
set yl "magnitude [mag]"

load "T0.plt"

set yr [:] reverse
#set ytics shift
set grid ytics
set key right
set mouse format "%.6f"

set arrow from T0-2400000,graph 0 rto 0,graph 1 nohead lt 0 front

p \
  "lightcurve2.dat" u ($1-2400000):($2+($3-band)*shift) w lp pt 2 lc 'cyan' t "no-zero-point",\
  "<awk '(NF==0){ i=0; }(NF>0){ i++; }(i==1)' chi2_LC2.dat" u ($1-2400000):($2+($4-band)*shift):3 w lp  pt 1 lc 'orange' t "synthetic",\
  "<awk '(NF==0){ i=0; }(NF>0){ i++; }(i==2)' chi2_LC2.dat" u ($1-2400000):($2+($4-band)*shift):3 w err pt 1 ps 0.5 lc 'blue' t "observed",\
  "chi2_LC2.dat" u ($1-2400000):($2+($4-band)*shift):3 w l lw 2 lc 'red' t "residua",\

pa -1

set term png small size 2048,1024
set out "chi2_LC2.png"
rep

q


  "Lc.dat"         u ($1-2400000):2:3 w err lt 3 pt 1 ps 1.5 t "observed",\
  "Lc_G.dat"       u ($1-2400000):2:3 w err lt 3 pt 1 ps 1.5 lc 'green',\
  "Lc_G.dat"       u ($1-2400000):($2 + 0.158924424):3 w err pt 1 ps 1.5 lw 3 lc 'cyan',\
  "Lc_V.dat"       u ($1-2400000):2:3 w err lt 3 pt 1 ps 3.5 lc 'blue',\


