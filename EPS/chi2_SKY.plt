#!/usr/bin/gnuplot

set ang rad
deg = pi/180.
rad = 180./pi

AU = 1.49597870691e11  # m
pc = 1./tan(1.*deg/3600.)  # AU
d = 64.1*pc  # pc

f(x) = atan(x/d)*rad*3600.*1.e3

########################################################################

set term post eps enh color dashed
set out "chi2_SKY.eps"
set size 0.5,0.7

set xl "{/Helvetica-Oblique x}_{pC} [mas]"
set yl "{/Helvetica-Oblique y}_{pC} [mas]" offset +1,0
set size ratio -1

set xr [-300:700]
set yr [-400:600]
set mxtics 2
set mytics 2
set zeroaxis
set key bottom right samplen 1.0 width -2.5

load "config.plt"

set bmargin 3.2
set tmargin 0.5
set lmargin 7.0
set rmargin 0.1

x1=-17.5
x2=-x1
y1=-20
y2=15

p \
  "<awk '($2==-3)' out_JDATE_photocentric.dat"  u (f($3)):(f($4)) t "B" w l ls 3,\
  "<awk '($2==-4)' out_JDATE_photocentric3.dat" u (f($3)):(f($4)) t "C" w l ls 4,\
  "<awk '($7==4)' arcsec_AU.dat" u (f($2)):(f($3)) t "{/=10 observ.}" w p ls 8,\
  "<awk '($7==4)' arcsec_AU.dat | ./ellipses.awk" u (f($1)):(f($2)) not w l ls 8,\
  "chi2_SKY.dat" u (f($2)):(f($3)) t "{/=10 residua}" w l ls 9,\
  "box.dat" u (x1+$1*(x2-x1)):(y1+$2*(y2-y1)) not w l lt 0
#pa -1

q

  "<awk '($2==-1)' out_JDATE_photocentric.dat"  u (f($3)):(f($4)) t "1" w l ls 1,\
  "<awk '($2==-2)' out_JDATE_photocentric.dat"  u (f($3)):(f($4)) t "2" w l ls 2,\
  "<awk '($6==3)' arcsec_AU.dat" u (f($2)):(f($3)):(f($4)):(f($5)) not                w xyerr ls 3,\
