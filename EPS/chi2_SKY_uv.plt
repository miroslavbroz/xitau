#!/usr/bin/gnuplot

deg = pi/180.
arcsec = deg/3600.
au = 1.49597870700e11  # m, from IAU 2012
pc = 648000.0/pi*au  # m, from IAU 2015
km = 1.e3  # m

set term post eps enh color solid "Helvetica" 18
set out "chi2_SKY_uv.eps"
set size 0.87,1.1

set colors classic

set tit "{/=12 viewing geometry is changing...}" offset 0,-0.5
set xl "{/Helvetica-Oblique u} [km]"
set yl "{/Helvetica-Oblique v} [km]" offset +1,0

tmp=750.
set xr [-tmp:tmp]
set yr [-tmp:tmp]
set size ratio -1
set zeroaxis
set ang rad
set key at graph -0.00,0.98 left font "Helvetica,14" samplen 1.0 spacing 1.1
set grid noxtics noytics front

set cbl "body" offset -1.5,0
set cbr [2:3]
set cbtics 1
set palette defined (\
  0.0 '#ff0000',\
  1.0 '#ffaa00' \
  )
set colorbox user size 0.02,0.9520 origin 0.80,0.1050

load "T0.plt"
d_pc = x_param26

fac = 0.10
x1 = 0.0
y1 = 0.0
a1 = 5.*km/km
x2 = x1
y2 = y1
a2 = 3.63e-3*arcsec*(d_pc*pc)/km
b2 = a2
x3 = x1
y3 = y1
a3 = 10.e-3*arcsec*(d_pc*pc)/km
b3 = a3
print "a1 = ", a1, " km (10-km)"
print "a2 = ", a2, " km (pixel size)"
print "a3 = ", a3, " km (PSF)"

set lmargin 7.5
set rmargin 5.0
set bmargin 3.0
set tmargin 1.3

f(x) = x*arcsec*(d_pc*pc)/km
g(x) = x*au/km

p \
  "<awk '($2==-3)' out_JDATE_uvw.dat" u (g($3)):(g($4)) t "3" w l lt 3 lc '#00ccff',\
  "<awk '($2==-2)' out_JDATE_uvw.dat" u (g($3)):(g($4)) t "2" w l lt 2 lc '#00ff66',\
  "<awk '(FNR>1){ print $0,ARGIND; }' nodes001.dat" u (f($2)):(f($3)) not w d lc 'gray',\
  sprintf("<awk '($2==-3) && ($1==%.10f)' out_JDATE_uvw.dat", T0) u (g($3)):(g($4)) t "{/Helvetica-Oblique T}_0" w p lc 0 pt 1 ps 2,\
  sprintf("<awk '($2==-2) && ($1==%.10f)' out_JDATE_uvw.dat", T0) u (g($3)):(g($4)) not     w p lc 0 pt 1 ps 2,\
  "arcsec_AU.dat" u (g($2)):(g($3)) t "observ." w p lt 7 pt 1 ps 0.5,\
  "<./ellipses.awk arcsec_AU.dat" u (g($1)):(g($2)) not w l lt 7,\
  "chi2_SKY.dat" u (-f($2)*sin($3*deg)):(f($2)*cos($3*deg)):8 t "residua" w l lc palette z lw 3,\

q


  "<awk '($2==-1)' ../out_JDATE_uvw.dat" u 3:4 t "1" w l lt 1 lc '#ff0000',\
  sprintf("<awk '($2==-1) && ($1==%.10f)' ../out_JDATE_uvw.dat", T0) u 3:4 t "T_0" w p lc 0 pt 1 ps 2,\

  sprintf("<../ellipse2.awk %.6e %.6e %.6e %.6e", x3, y3, a3, b3) u 1:2 t "PSF (3-s.)" w l lc 'black' dt 2
  sprintf("<../ellipse2.awk %.6e %.6e %.6e %.6e", x1, y1, a1, a1) u 1:2 t "10 km" w l lc 'black',\
  "../box.dat" u (x2-a2/2+a2*$1):(y2-b2/2+b2*$2) t "1 pixel" w l lc 'gray',\
  "../arcsec_AU.dat" u 2:3:(sprintf("  %.0f", $1-2400000)) not w labels left font "Helvetica,10" tc 'gray',\

