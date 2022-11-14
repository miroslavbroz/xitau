#!/usr/bin/gnuplot

deg = pi/180.
arcsec = deg/3600.
au = 1.49597870700e11  # m, from IAU 2012
pc = 648000.0/pi*au  # m, from IAU 2015
km = 1.e3  # m

set colors classic
set term x11

set xl "u [arcsec]"
set yl "v [arcsec]"
set cbl "chi_  []"

tmp=1.0
set xr [-tmp:tmp]
set yr [-tmp:tmp]
set cbr [0:10]
set size ratio -1
set zeroaxis
set palette rgbformulae 33,13,10

f(x,d) = x/d/arcsec

p "variables.tmp" u 2:3:4 lc palette z,\
  "variables.tmp__47.6" u 2:3 w p ps 2 lw 2 lc 'black',\
  "<./ellipses.awk arcsec_AU.dat" u (f($1,$3)):(f($2,$3)) not w l lt 7,\

pa -1

set term png small size 2048,2048
set out "variables.png"
rep

q

  "variables.tmp__80.6" u 2:3 w p ps 2 lw 2 lc 'black',\


