#!/usr/bin/gnuplot

deg = pi/180.
arcsec = deg/3600.
au = 1.49597870700e11  # m, from IAU 2012
pc = 648000.0/pi*au  # m, from IAU 2015

set colors classic
set term x11

set tit "viewing geometry is changing..."
set xl "Du [arcsec/day]"
set yl "Dv [arcsec/day]"

tmp=4.
set xr [-tmp:tmp]
set yr [-tmp:tmp]
set size ratio -1
set zeroaxis
set ang rad

load "T0.plt"

set cbl "ibod"
set cbr [2:3]
set cbtics 1
set palette defined (\
  0.0 '#ff0000',\
  1.0 '#ffaa00' \
  )

p \
  "chi2_SKY3.dat" u 2:3:9 t "residua" w l lw 3 lc palette z,\
  "<awk '!/#/ && (NF>0){ i++; }(i==2){ print; }(NF==0){ i=0; }' chi2_SKY3.dat | ./ellipses.awk" not w l lw 1 lt 7,\
  "<awk '!/#/ && (NF>0){ i++; }(i==1){ print; }(NF==0){ i=0; }' chi2_SKY3.dat" u (0):(0):2:3 t "synthetic" w vectors lc 'gray',\
  "<awk '!/#/ && (NF>0){ i++; }(i==2){ print; }(NF==0){ i=0; }' chi2_SKY3.dat" u (0):(0):2:3 t "observed"  w vectors lc 'black',\
  "<awk '!/#/ && (NF>0){ i++; }(i==2){ print; }(NF==0){ i=0; }' chi2_SKY3.dat" u 2:3:(sprintf("  %.0f", $1-2400000)) not w labels left,\

pa -1

set term png small
set out "chi2_SKY3.png"
rep

q
  "arcsec_AU3.dat" u 2:3 t "observ." w p lt 7 pt 1 ps 0.5,\
  "arcsec_AU3.dat" u 2:3:(sprintf("  %.0f", $1-2400000)) not w labels left,\

  sprintf("<awk '($2==-2){ x=$3; y=$4; }($2==-3) && ($1==%.10f){ print $3-x,$4-y; }' out_JDATE_uvw.dat", T0) u 1:2 t "T_0" w p lc 0 pt 1 ps 2,\
  "<awk '($2==-2){ x=$3; y=$4; }($2==-3){ print $3-x,$4-y; }' out_JDATE_uvw.dat" u 1:2 t "3-2" w l lt 3,\

  sprintf("<awk '($2==-2){ x=$3; y=$4; }($2==-3) && ($1==%.10f){ print $3-x,$4-y; }' out_JDATE_uvw.dat", 2457948.709022) u 1:2 not w p lc 'black' pt 6 ps 2,\
