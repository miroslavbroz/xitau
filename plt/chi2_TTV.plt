#!/usr/bin/env gnuplot

load "T0.plt"

set colors classic
#set term x11

#day = 1440.  # min
day = 1.  # day

set xl "JD - 2400000"
#set yl "O-C [min]"
set yl "O-C [day]"
set y2l "lite [day]"
set ytics nomirror
set y2tics

#set xr [56000:60500]
#set xr [T0-2400000-10:T0-2400000+40]
tmp=0.0035; set yr [-tmp:tmp]
set zeroaxis
set key left
set palette rgbformulae 33,13,10

set arrow from T0-2400000,graph 0 to T0-2400000,graph 1 nohead lt 0

p "minima.dat" u ($1-2400000):(0.0) t "synthetic minima (primary and secondary)" w p lc 'gray' pt 1 ps 1.0,\
  "Omc12.dat" u ($1-2400000):(0.0) t "observed minima" w p lc 'green' pt 2 ps 2,\
  "chi2_TTV.dat" u ($1-2400000):($3*day):($4*day) t "O-C" w err lc 'blue' lw 1 pt 1 ps 0.5,\
  "<awk '!/^#/{ print $1,0.0; print $1,$3; print s; }' chi2_TTV.dat" u ($1-2400000):($2*day) t "residua" w l lc 'red' lw 3,\
  "<awk '($6>100.0)' chi2_TTV.dat" u ($1-2400000):($3*day) t "chi^2 > 10.0" w p pt 6 ps 2 lc 'red',\
  "<awk '($2==1)' lite.dat" u ($1-2400000):3:2 ax x1y2 w l lc palette z not,\
  "<awk '($2==2)' lite.dat" u ($1-2400000):3:2 ax x1y2 w l lc palette z not,\
  "<awk '($2==3)' lite.dat" u ($1-2400000):3:2 ax x1y2 w l lc palette z not,\
  "<awk '($2==4)' lite.dat" u ($1-2400000):3:2 ax x1y2 w l lc palette z not,\
  "<echo 0 0" u (T0-2400000):(0.0) ax x1y2 w p pt 1 ps 2 lc 'gray' not

#  "utctolite.tmp" u ($1-2400000):($2*day) w lp lc 'green' t "heliocentric correction (lite)"

pa -1

set term png small size 2048,1024
set out "chi2_TTV.png"
rep

q

  "Omc12.dat" u ($1-2400000):($3*day) w p t "Mayer etal., Fig. 9" lc 'gray'


