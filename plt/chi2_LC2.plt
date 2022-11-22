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

set arrow from 2459546.7125847600-2400000,graph 0 rto 0,graph 1 nohead lt 0  # 19
set arrow from 2459546.8038658402-2400000,graph 0 rto 0,graph 1 nohead lt 0  # 89
set arrow from 2459548.5640605805-2400000,graph 0 rto 0,graph 1 nohead lt 0  # 1
set arrow from 2459548.6379006002-2400000,graph 0 rto 0,graph 1 nohead lt 0  # 66

set label "OCC" at 2459546.680844-2400000,graph 0.95 center
set label "REF" at 2459547.720452-2400000,graph 0.95 center
set label "TRA" at 2459548.552881-2400000,graph 0.95 center
set label "REF" at 2459548.668864-2400000,graph 0.95 center
set label "OCC" at 2459555.667224-2400000,graph 0.95 center
set label "TRA" at 2459556.723283-2400000,graph 0.95 center
set label "OCC" at 2459557.415516-2400000,graph 0.95 center
set label "REF" at 2459559.384698-2400000,graph 0.95 center

p \
  "lightcurve2.dat" u ($1-2400000):($2+($3-band)*shift) w lp pt 2 lc 'cyan' t "no-zero-point",\
  "<awk '(NF==0){ i=0; }(NF>0){ i++; }(i==1)' chi2_LC2.dat" u ($1-2400000):($2+($4-band)*shift):3 w lp  pt 1 lc 'orange' t "synthetic",\
  "chi2_LC2.dat" u ($1-2400000):($2+($4-band)*shift):3 w l lw 2 lc 'red' t "residua",\
  "<awk '(NF==0){ i=0; }(NF>0){ i++; }(i==2)' chi2_LC2.dat" u ($1-2400000):($2+($4-band)*shift):3 w err pt 1 ps 0.5 lc 'blue' t "observed",\

pa -1

set term png small size 2048,1024
set out "chi2_LC2.png"
rep

q

  "<awk '!/^ *#/{ i++; print $1,$2,i; }' lightcurve2.dat" u ($1-2400000):2:3 w labels not,\

  "Lc.dat"         u ($1-2400000):2:3 w err lt 3 pt 1 ps 1.5 t "observed",\
  "Lc_G.dat"       u ($1-2400000):2:3 w err lt 3 pt 1 ps 1.5 lc 'green',\
  "Lc_G.dat"       u ($1-2400000):($2 + 0.158924424):3 w err pt 1 ps 1.5 lw 3 lc 'cyan',\
  "Lc_V.dat"       u ($1-2400000):2:3 w err lt 3 pt 1 ps 3.5 lc 'blue',\


