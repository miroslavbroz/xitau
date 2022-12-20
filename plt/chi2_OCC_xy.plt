#!/usr/bin/gnuplot

deg=pi/180.

set xl "x"
set yl "y"

tmp=0.14
set xr [-tmp:tmp]
set yr [-tmp:tmp]
set cbr [1:3]
set size ratio -1
set view equal xy
set zeroaxis
set grid front
unset grid
set key bottom
set nocolorbox
set palette defined (\
  0.0 '#00ffff',\
  0.5 '#00ff00',\
  1.0 '#0000ff' \
  )
set tics front
set grid front

# https://mathworld.wolfram.com/GnomonicProjection.html

lambda0 = 140.68583300*deg
phi0 = (36.64250000+0)*deg
f(x) = abs(x*deg-lambda0) < 90*deg ? x*deg : NaN
g(x) = x*deg
fx(lambda,phi) = cos(g(phi))*sin(f(lambda)-lambda0)
fy(lambda,phi) = (cos(phi0)*sin(g(phi))-sin(phi0)*cos(g(phi))*cos(f(lambda)-lambda0))/(sin(phi0)*sin(g(phi))+cos(phi0)*cos(g(phi))*cos(f(lambda)-lambda0))

p \
  "xitau/plt/world_50m.txt" u (fx($1,$2)):(fy($1,$2)) w filledcurves lc '#e3e3e3' not,\
  "xitau/plt/world_50m.txt" u (fx($1,$2)):(fy($1,$2)) w l lc 'black' not,\
  "xitau/plt/bound_50m.txt" u (fx($1,$2)):(fy($1,$2)) w l lc '#999999' not,\
  "occultation3.dat" u (fx($2,$3)):(fy($2,$3)) w l lw 2 lc 'black' t "shadow",\
  "<awk '($4==1)' occultation.dat" u (fx($2,$3)):(fy($2,$3)):4 w lp pt 7 lc palette z t "(22)",\
  "<awk '($4==2)' occultation.dat" u (fx($2,$3)):(fy($2,$3)):4 w lp pt 7 lc palette z t "Linus",\
  "<awk '($4==3)' occultation.dat" u (fx($2,$3)):(fy($2,$3)):4 w lp pt 7 lc palette z t "outer moon",\
  "<awk '!/^ #/{ i++; }(NF==0){ i=0; }(i==2)' chi2_OCC.dat" u (fx($3,$4)):(fy($3,$4)) w p pt 1 ps 2 lc 'blue'   t "observed",\
  "<awk '!/^ #/{ i++; }(NF==0){ i=0; }(i==1)' chi2_OCC.dat" u (fx($3,$4)):(fy($3,$4)) w p pt 2 ps 2 lc 'orange' t "synthetic",\
  "<awk '!/^ #/{ i++; }(NF==0){ i=0; }(i==1)' chi2_OCC.dat" u (fx($3,$4)):(fy($3,$4)):(sprintf("  %s", stringcolumn(8))) w labels not left,\
  "chi2_OCC.dat" u (fx($3,$4)):(fy($3,$4)) w l lw 3 lc 'red' t "residua",\

pa -1

set term png small size 1024,1024
set out "chi2_OCC_xy.png"
rep

q



