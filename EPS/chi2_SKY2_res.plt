#!/usr/bin/gnuplot

set xl "{/Helvetica-Oblique JD} - epoch (shifted)"
set yl "residua {/Helvetica-Oblique u}, {/Helvetica-Oblique v} [arcsec]" offset +1,0

set xr [-0:150]
set xtics 25
tmp=0.05
set yr [-tmp:tmp]
set zeroaxis
set key left samplen 1.5

set cbl "body"
set cbr [2:3]
set cbtics 1
set palette defined (\
  0.0 '#ff0000',\
  1.0 '#ffaa00' \
  )

load "T0.plt"

T1=2454728
T2=2457948
T3=2458462

f(x) = \
  x > T3 ? x-T3+100 : \
  x > T2 ? x-T2+50 : \
  x-T1+0

set arrow from T0-T1,graph 0 rto 0,graph 1 nohead lt 0
set arrow from 50,graph 0 rto 0,graph 1 nohead lt 0
set arrow from 100,graph 0 rto 0,graph 1 nohead lt 0

set label "{/=14 DESCAMPS}"   at  25,graph 0.08 center
set label "{/=14 SPHERE2017}" at  75,graph 0.08 center
set label "{/=14 SPHERE2018}" at 125,graph 0.08 center

set lmargin 7.5
set rmargin 1.6
set bmargin 3.1
set tmargin 0.7

p \
  "<awk 'BEGIN{ pi=3.1415926535; deg=pi/180.; }!/^#/ && (NF>0){ t=$1; x=$2; }(i==0){ x0=x; }(i==1){ print t,0,$8; print t,x-x0,$8; print null; }{ i++; }(NF==0){ i=0; }' chi2_SKY2.dat" u (f($1)+$3-2):2:3 t "{/Helvetica-Oblique u}" w l lc 'red'     lw 6 dt 1,\
  "<awk 'BEGIN{ pi=3.1415926535; deg=pi/180.; }!/^#/ && (NF>0){ t=$1; y=$3; }(i==0){ y0=y; }(i==1){ print t,0,$8; print t,y-y0,$8; print null; }{ i++; }(NF==0){ i=0; }' chi2_SKY2.dat" u (f($1)+$3-2):2:3 t "{/Helvetica-Oblique v}" w l lc 'green'   lw 2 dt 2,\

pa -1

set term post eps enh color dashed "Helvetica,18"
set out "chi2_SKY2_res.eps"
set size 0.80,0.48
rep

q


