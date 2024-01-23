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
tmp=1.6
set xr [-tmp:tmp]
set yr [-tmp:tmp]
set xtics 0.1
set ytics 0.1
set size ratio -1
set zeroaxis
set ang rad
set key left

load "T0.plt"

P1 = x_param4
#d_pc = x_param22
#d_pc = x_param35

set cbl "ibod"
set cbr [0:10]
set cbtics 1
set palette defined (\
  0.0 '#ccffcc',\
  0.5 '#ffcc00',\
  1.0 '#ff0000' \
  )

#x1 = 0.0
#y1 = 0.0
#a1 = 5.*km/(d_pc*pc)/arcsec
#x2 = x1
#y2 = y1
#a2 = 3.63e-3*arcsec/arcsec
#b2 = a2
#x3 = x1
#y3 = y1
#a3 = 8*a2/2.
#b3 = a3
#print "a1 = ", a1, " arcsec (5 km)"
#print "a2 = ", a2, " arcsec (1 pixel)"
#print "a3 = ", a3, " arcsec (PSF)"

f(x,d) = x/d/arcsec

p \
  "<awk '($2==-4)' out_JDATE_uvw.dat" u (f($3,$6)):(f($4,$6)) t "4" w l lc 'cyan',\
  "<awk '($2==-3)' out_JDATE_uvw.dat" u (f($3,$6)):(f($4,$6)) t "3" w l lc 'blue',\
  "<awk '($2==-2)' out_JDATE_uvw.dat" u (f($3,$6)):(f($4,$6)) t "2" w l lc 'green',\
  "<awk '($2==-1)' out_JDATE_uvw.dat" u (f($3,$6)):(f($4,$6)) t "1" w l lc 'red',\
  sprintf("<awk '($2==-1) && ($1>=%.10f) && ($1<=%.10f)' out_JDATE_uvw.dat", T0, T0+0.1) u (f($3,$6)):(f($4,$6)) not w l lw 10 lc 'gray',\
  sprintf("<awk '($2==-2) && ($1>=%.10f) && ($1<=%.10f)' out_JDATE_uvw.dat", T0, T0+0.1) u (f($3,$6)):(f($4,$6)) not w l lw 10 lc 'gray',\
  sprintf("<awk '($2==-3) && ($1>=%.10f) && ($1<=%.10f)' out_JDATE_uvw.dat", T0, T0+0.1) u (f($3,$6)):(f($4,$6)) not w l lw 10 lc 'gray',\
  sprintf("<awk '($2==-4) && ($1>=%.10f) && ($1<=%.10f)' out_JDATE_uvw.dat", T0, T0+0.1) u (f($3,$6)):(f($4,$6)) not w l lw 10 lc 'gray',\
  sprintf("<awk '($2==-1) && ($1==%.10f)' out_JDATE_uvw.dat", T0) u (f($3,$6)):(f($4,$6)) t "T_0" w p lc 0 pt 1 ps 2,\
  sprintf("<awk '($2==-2) && ($1==%.10f)' out_JDATE_uvw.dat", T0) u (f($3,$6)):(f($4,$6)) not     w p lc 0 pt 1 ps 2,\
  sprintf("<awk '($2==-3) && ($1==%.10f)' out_JDATE_uvw.dat", T0) u (f($3,$6)):(f($4,$6)) not     w p lc 0 pt 1 ps 2,\
  sprintf("<awk '($2==-4) && ($1==%.10f)' out_JDATE_uvw.dat", T0) u (f($3,$6)):(f($4,$6)) not     w p lc 0 pt 1 ps 2,\
  "<./ellipses.awk arcsec_AU.dat" u (f($1,$3)):(f($2,$3)) not w l lc 'black',\
  "chi2_SKY.dat" u (-$2*sin($3*deg)):($2*cos($3*deg)):9 t "residua" w l lc palette z lw 3,\
  "<awk '($NF+0>100)' chi2_SKY.dat" u (-$2*sin($3*deg)):($2*cos($3*deg)):8 t "chi^2 > 100" w p lt 1 pt 6 ps 1.5 lw 3,\
  "arcsec_AU.dat" u (f($2,$7)):(f($3,$7)) t "observ." w p lt 7 pt 1 ps 0.5,\
  "<awk '' arcsec_AU.dat" u (f($2,$7)):(f($3,$7)) t "observ." w p lt 7 pt 1 ps 0.5,\
  "arcsec_AU.dat" u (f($2,$7)):(f($3,$7)):(sprintf("  %.0f  ", int($1-2400000))) not w labels left,\
  "<awk '($1>=2457021.56786759-0.001) && ($1<2457021.56786759+0.001)' arcsec_AU.dat" u (f($2,$7)):(f($3,$7)):(sprintf("  %.0f  ", int($1-2400000))) not w labels left tc 'blue',\
  "<awk '(FNR>1){ print $0,ARGIND; }' nodes0001.dat" u 2:3 t "shape" w d lc 'black',\

pa -1

set term png small size 2048,2048
set out "chi2_SKY_uv.png"
rep

q

  "chi2_SKY.dat" u (-$2*sin($3*deg)):($2*cos($3*deg)):8 t "residua" w l lc palette z lw 3,\
  sprintf("<awk '($2==-2) && ($1>=%.10f) && ($1<=%.10f) && ($5<0.0)' out_JDATE_uvw.dat", T0-P1, T0+P1) u (f($3,$6)):(f($4,$6)) not w p pt 5 lc 'green',\
  sprintf("<./ellipse2.awk %.6e %.6e %.6e %.6e", x3, y3, a3, b3) u 1:2 t "PSF (3-s.)" w l lc 'black' dt 2,\
  sprintf("<./ellipse2.awk %.6e %.6e %.6e %.6e", x1, y1, a1, a1) u 1:2 t "10 km" w l lc 'black',\
  "box.dat" u (x2-a2/2+a2*$1):(y2-b2/2+b2*$2) t "1 pixel" w l lc 'black',\
  "arcsec_AU.dat" u (f($2,$7)):(f($3,$7)):(sprintf("  %.0f", $1-2400000)) not w labels left,\

