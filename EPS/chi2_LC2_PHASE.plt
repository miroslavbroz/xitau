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

set xl "phase [1]"
set yl "{/Helvetica-Oblique R}_c [mag]" offset 2,0
set cbl "JD {/Symbol -} 2400000" offset -1,0

load "T0.plt"

set yr [10.85:10.15]
set bar 0.0
set cbr [2459546.6921026399-2400000:2459548.7716091899-2400000]
set cbtics 0.5 offset -1,0 font "Helvetica,14"
unset colorbox
set palette defined (\
  0.0 "blue",\
  0.8 "#0080ff",\
  1.0 "cyan" \
  )
set key right bottom samplen 2.0 spacing 1.2 font "Helvetica,14"

p \
  "<awk '(NF==0){ i=0; }(NF>0){ i++; }(i==1)' chi2_LC2.dat" u (phase($1)):($2+($4-band)*shift):3 w p  pt 1 lc 'orange' t "synthetic",\
  "chi2_LC2.dat" u (phase($1)):($2+($4-band)*shift):3 w l lw 3 lc 'red' t "residua",\
  "<awk '(NF==0){ i=0; }(NF>0){ i++; }(i==2)' chi2_LC2.dat" u (phase($1)):($2+($4-band)*shift):3:($1-2400000) w err pt 1 ps 0.5 lc palette z t "observed",\

#pa -1

set term post eps enh color solid "Helvetica" 18
set out "chi2_LC2_PHASE.eps"
set size 0.85,0.8
set lmargin 6.5
set rmargin 0.6
set bmargin 3.0
set tmargin 0.6
rep

q

  "lightcurve2.dat" u (phase($1)):($2+($3-band)*shift) w p pt 2 lc 'cyan' t "no-zero-point",\
  "<awk '!/^ *#/ && (i<100){ i++; print $1,$2,i; }' lightcurve2.dat" u (phase($1)):($2+0.02):3 w labels not,\

  "Lc.dat"         u ($1-2400000):2:3 w err lt 3 pt 1 ps 1.5 t "observed",\
  "Lc_G.dat"       u ($1-2400000):2:3 w err lt 3 pt 1 ps 1.5 lc 'green',\
  "Lc_G.dat"       u ($1-2400000):($2 + 0.158924424):3 w err pt 1 ps 1.5 lw 3 lc 'cyan',\
  "Lc_V.dat"       u ($1-2400000):2:3 w err lt 3 pt 1 ps 3.5 lc 'blue',\



