#!/usr/bin/gnuplot

set colors classic
set term x11

band = 7
shift = 0.0

set xl "JD - 2400000"
set yl "magnitude [mag]"

load "T0.plt"

#set xr [59548.5:]
set yr [:] reverse
#set ytics shift
set grid ytics
set key right
set mouse format "%.6f"

set arrow from T0-2400000,graph 0 rto 0,graph 1 nohead lt 0 front

set arrow from (2459880.6675-2400000),graph 0 rto 0,graph 1 nohead lt 0
set arrow from (2459881.9175-2400000),graph 0 rto 0,graph 1 nohead lt 0
set arrow from (2459883.2091-2400000),graph 0 rto 0,graph 1 nohead lt 0
set arrow from (2459884.5008-2400000),graph 0 rto 0,graph 1 nohead lt 0
set arrow from (2459885.7508-2400000),graph 0 rto 0,graph 1 nohead lt 0
set arrow from (2459887.0425-2400000),graph 0 rto 0,graph 1 nohead lt 0
set arrow from (2459888.3341-2400000),graph 0 rto 0,graph 1 nohead lt 0
set arrow from (2459889.5841-2400000),graph 0 rto 0,graph 1 nohead lt 0

#set label "REF"         at 2454165.50793978-2400000,graph 0.95 center
#set label "ECL of L."   at 2454167.50085480-2400000,graph 0.95 center
#set label "ECL of (22)" at 2454176.56817257-2400000,graph 0.95 center
#set label "REF"         at 2454177.56877556-2400000,graph 0.95 center
#set label "OCC of L."   at 2459546.680844-2400000,graph 0.95 center
#set label "REF"         at 2459547.720452-2400000,graph 0.95 center
#set label "TRA of L."   at 2459548.552881-2400000,graph 0.95 center
#set label "REF"         at 2459548.668864-2400000,graph 0.95 center
#set label "OCC"         at 2459555.667224-2400000,graph 0.95 center
#set label "TRA"         at 2459556.723283-2400000,graph 0.95 center
#set label "OCC"         at 2459557.415516-2400000,graph 0.95 center
#set label "REF"         at 2459559.384698-2400000,graph 0.95 center

T1=2459546.7663
P1=3.6017793629369406

set label "OCC of L."         at T1+0.0*P1-2400000,graph 0.95 center
set label "OCC of L."         at T1+1.0*P1-2400000,graph 0.95 center
set label "OCC of L."         at T1+2.0*P1-2400000,graph 0.95 center
set label "OCC of L."         at T1+3.0*P1-2400000,graph 0.95 center
set label "OCC of L."         at T1+4.0*P1-2400000,graph 0.95 center
set label "TRA of L."         at T1+0.5*P1-2400000,graph 0.95 center
set label "TRA of L."         at T1+1.5*P1-2400000,graph 0.95 center
set label "TRA of L."         at T1+2.5*P1-2400000,graph 0.95 center
set label "TRA of L."         at T1+3.5*P1-2400000,graph 0.95 center
set label "TRA of L."         at T1+4.5*P1-2400000,graph 0.95 center

p \
  "lightcurve2.dat" u ($1-2400000):($2+($3-band)*shift) w lp pt 2 lc 'cyan' t "no-zero-point",\
  "<awk '(NF==0){ i=0; }(NF>0){ i++; }(i==1)' chi2_LC2.dat" u ($1-2400000):($2+($4-band)*shift):3 w lp  pt 1 lc 'orange' t "synthetic",\
  "chi2_LC2.dat" u ($1-2400000):($2+($4-band)*shift):3 w l lw 2 lc 'red' t "residua",\
  "<awk '(NF==0){ i=0; }(NF>0){ i++; }(i==2)' chi2_LC2.dat" u ($1-2400000):($2+($4-band)*shift):3 w err pt 1 ps 0.5 lc 'blue' t "observed",\
  "<awk '!/^ *#/ && (i<100){ i++; print $1,$2,i; }' lightcurve2.dat" u ($1-2400000):($2+0.02):3 w labels not,\

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


