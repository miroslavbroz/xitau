#!/usr/bin/gnuplot

deg = pi/180.
arcsec = deg/3600.
au = 1.49597870700e11  # m, from IAU 2012
pc = 648000.0/pi*au  # m, from IAU 2015
km = 1.e3  # m

set colors classic
set term x11

set tit "viewing geometry is changing..."
set xl "u [arcsec]"
set yl "v [arcsec]"

tmp=0.8
set xr [-tmp:tmp]
set yr [-tmp:tmp]
set size ratio -1
set zeroaxis
set ang rad
set key left

load "T0.plt"

d_pc = x_param26
f(x) = x/((d_pc*pc)/au)/arcsec

set cbl "ibod"
set cbr [2:3]
set cbtics 1
set palette defined (\
  0.0 '#ff0000',\
  1.0 '#ffaa00' \
  )

fac = 0.10
x1 = 0.0
y1 = 0.0
r1 = 5.*km/(d_pc*pc)/arcsec
print "r1 = ", r1
x2 = x1
y2 = y1
a2 = 3.63e-3
b2 = a2
print "a2 = ", a2
x3 = x1
y3 = y1
a3 = 8*a2/2.
b3 = a3
print "a3 = ", a3

p \
  "<awk '($2==-2)' out_JDATE_uvw.dat" u (f($3)):(f($4)) t "2" w l lt 2,\
  "<awk '($2==-3)' out_JDATE_uvw.dat" u (f($3)):(f($4)) t "3" w l lc '#00ccff',\
  "chi2_SKY3.dat" u ($10+fac*$2):($11+fac*$3):9 t "residua" w l lc palette z lw 3,\
  "<awk '!/#/ && (NF>0){ i++; }(i==1){ print; }(NF==0){ i=0; }' chi2_SKY3.dat" u 10:11:(fac*$2):(fac*$3) t "synthetic" w vectors lc '#999999',\
  "<awk '!/#/ && (NF>0){ i++; }(i==2){ print; }(NF==0){ i=0; }' chi2_SKY3.dat" u 10:11:(fac*$2):(fac*$3) t "observed"  w vectors lc 'black',\
  "<awk '!/#/ && (NF>0){ i++; }(i==2){ print; }(NF==0){ i=0; }' chi2_SKY3.dat" u ($10+fac*$2):($11+fac*$3):(sprintf("  %.0f", $1-2400000)) not w labels left,\
  sprintf("<./ellipse2.awk %.6e %.6e %.6e %.6e", x3, y3, a3, b3) u 1:2 t "PSF (3-sigma)" w l lc 'black' dt 2,\
  sprintf("<./ellipse2.awk %.ef %.ef %.ef %.ef", x1, y1, r1, r1) u 1:2 t "10 km" w l lc 'black',\
  "box.dat" u (x2-a2/2+a2*$1):(y2-b2/2+b2*$2) t "1 pixel" w l lc 'gray'

pa -1

set term png small size 1024,1024
set out "chi2_SKY3_uv.png"
rep

q


  "<awk '($2==-1)' out_JDATE_uvw.dat" u (f($3)):(f($4)) t "1" w l lt 1,\
  "<awk '($2==-4)' out_JDATE_uvw.dat" u (f($3)):(f($4)) t "4" w l lt 4,\

  "<awk '!/#/ && (NF>0){ i++; }(i==2){ print; }(NF==0){ i=0; }' chi2_SKY3.dat | ./ellipses.awk" w l lw 1 lt 7,\

  "arcsec_AU3.dat" u 2:3 t "observ." w p lt 7 pt 1 ps 0.5,\
  "arcsec_AU3.dat" u 2:3:(sprintf("  %.0f", $1-2400000)) not w labels left,\

  sprintf("<awk '($2==-2){ x=$3; y=$4; }($2==-3) && ($1==%.10f){ print $3-x,$4-y; }' out_JDATE_uvw.dat", T0) u 1:2 t "T_0" w p lc 0 pt 1 ps 2,\
  "<awk '($2==-2){ x=$3; y=$4; }($2==-3){ print $3-x,$4-y; }' out_JDATE_uvw.dat" u 1:2 t "3-2" w l lt 3,\

  sprintf("<awk '($2==-2){ x=$3; y=$4; }($2==-3) && ($1==%.10f){ print $3-x,$4-y; }' out_JDATE_uvw.dat", 2457948.709022) u 1:2 not w p lc 'black' pt 6 ps 2,\
