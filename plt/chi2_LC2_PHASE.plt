#!/usr/bin/gnuplot

load "T0.plt"

JD0 = 2436258.0000000000
P = x_param4
P = 0.17284167583333299

f1(x) = x > 0.0 ? x : x+1.0
frac(x) = f1(x-int(x))
phase(jd) = frac((jd-JD0)/P)

set colors classic
set term x11

band = 7
band = 54
shift = 0.0

set xl "JD - 2400000"
set yl "magnitude [mag]"

load "T0.plt"

set yr [:] reverse
set cbr [2459546.6921026399-2400000:2459548.7716091899-2400000]
#set ytics shift
set grid ytics
set key right
set palette defined (\
  0.0 "blue",\
  0.8 "#0080ff",\
  1.0 "cyan" \
  )

set arrow from phase(2459546.7125847600),graph 0 rto 0,graph 1 nohead lt 0  # 19
set arrow from phase(2459546.8038658402),graph 0 rto 0,graph 1 nohead lt 0  # 89

p \
  "lightcurve2.dat" u (phase($1)):($2+($3-band)*shift) w p pt 2 lc 'cyan' t "no-zero-point",\
  "<awk '(NF==0){ i=0; }(NF>0){ i++; }(i==1)' chi2_LC2.dat" u (phase($1)):($2+($4-band)*shift):3 w p  pt 1 lc 'orange' t "synthetic",\
  "chi2_LC2.dat" u (phase($1)):($2+($4-band)*shift):3 w l lw 3 lc 'red' t "residua",\
  "<awk '(NF==0){ i=0; }(NF>0){ i++; }(i==2)' chi2_LC2.dat" u (phase($1)):($2+($4-band)*shift):3:($1-2400000) w err pt 1 ps 0.5 lc palette z t "observed",\
  "<awk '!/^ *#/ && (i<100){ i++; print $1,$2,i; }' lightcurve2.dat" u (phase($1)):($2+0.02):3 w labels not,\

pa -1

set term png small size 2048,1024
set out "chi2_LC2_PHASE.png"
rep

q

  "Lc.dat"         u ($1-2400000):2:3 w err lt 3 pt 1 ps 1.5 t "observed",\
  "Lc_G.dat"       u ($1-2400000):2:3 w err lt 3 pt 1 ps 1.5 lc 'green',\
  "Lc_G.dat"       u ($1-2400000):($2 + 0.158924424):3 w err pt 1 ps 1.5 lw 3 lc 'cyan',\
  "Lc_V.dat"       u ($1-2400000):2:3 w err lt 3 pt 1 ps 3.5 lc 'blue',\



