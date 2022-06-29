#!/usr/bin/gnuplot

deg = pi/180.
au = 1.49597870700e11  # m, from IAU 2012
R_E = 6.378173e6  # m; WGS-82 http://wiki.gis.com/wiki/index.php/Reference_ellipsoid 

set tit "fundamental plane"
set xl "u [R_E]"
set yl "v [R_E]"
set cbl "ibod"

tmp=1.5
set xr [tmp:-tmp]
set yr [-tmp:tmp]
set cbr [1:3]
set cbtics 1
set size ratio -1
set zeroaxis
set key left
#set nocolorbox
set palette defined (\
  0.0 '#00ff00',\
  0.5 '#ff0000',\
  1.0 '#ffaa00' \
  )

set parametric
set trange [0:360.0]
fx(r,t) = r*cos(t*deg)
fy(r,t) = r*sin(t*deg)

f(x) = x*au/R_E

p "occultation2.dat" u (f($2)):(f($3)):4 w lp lc palette z t "Xitau",\
  fx(1.0,t), fy(1.0,t) w l lt 0 not

pa -1

set term png small
set out "occultation2.png"
rep

q




