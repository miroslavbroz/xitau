#!/usr/bin/gnuplot

set xl "JD - epoch (shifted)"
set yl "residua u, v [arcsec]"

tmp=0.10
set yr [-tmp:tmp]
set zeroaxis

set cbl "ibod"
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
  x > T3 ? x-T3+200 : \
  x > T2 ? x-T2+100 : \
  x-T1+0

f(x) = x-2400000

#set arrow from T0-T1,graph 0 rto 0,graph 1 nohead lt 0
#set arrow from 100,graph 0 rto 0,graph 1 nohead lt 0
#set arrow from 200,graph 0 rto 0,graph 1 nohead lt 0
#set label "DESCAMPS"   at  50,graph 0.05 center
#set label "SPHERE2017" at 150,graph 0.05 center
#set label "SPHERE2018" at 225,graph 0.05 center
set arrow from 2454047.326750-2400000,graph 0 rto 0,graph 1 nohead lt 0

p \
  "<awk 'BEGIN{ pi=3.1415926535; deg=pi/180.; }!/^#/ && (NF>0){ t=$1; x=-$2*sin($3*deg); }(i==0){ x0=x; }(i==1){ print t,0,$8; print t,x-x0,$8; print null; }{ i++; }(NF==0){ i=0; }' chi2_SKY.dat" u (f($1)+$3-2):2:3 t "u" w l lc palette z lw 4 dt 1,\
  "<awk 'BEGIN{ pi=3.1415926535; deg=pi/180.; }!/^#/ && (NF>0){ t=$1; y= $2*cos($3*deg); }(i==0){ y0=y; }(i==1){ print t,0,$8; print t,y-y0,$8; print null; }{ i++; }(NF==0){ i=0; }' chi2_SKY.dat" u (f($1)+$3-2):2:3 t "v" w l lc 'green'   lw 2 dt 2,\


pa -1

set term png small
set out "chi2_SKY_res.png"
rep

q
  "<awk 'BEGIN{ pi=3.1415926535; deg=pi/180.; }!/^#/ && (NF>0){ t=$1; x=-$2*sin($3*deg); y=$2*cos($3*deg); }(i==0){ x0=x; y0=y; }(i==1){ print t,0,0; print t,x-x0,0; print null; print t,0,1; print t,y-y0,1; print null; }{ i++; }(NF==0){ i=0; }' chi2_SKY.dat" u ($1-2400000+10*$3):2:3 w l lc palette z lw 2

