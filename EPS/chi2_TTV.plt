#!/usr/bin/gnuplot

set colors classic
set term post eps enh color dashed
set out "chi2_TTV.eps"
set size 1.0,0.7

#day = 1440.  # min
day = 1.  # day

set xl "JD {/Symbol -} 2400000"
set yl "TTV [day] wrt. N-body model" offset +0.5,0

#set xr [59000:60500]
tmp = 0.25*day  # day
#set yr [-tmp:tmp]
set zeroaxis
set key left width +5 font "Helvetica,14"

load "T0.plt"
set arrow from T0-2400000,graph 0 to T0-2400000,graph 1 nohead lt 0

p "minima.dat" u ($1-2400000):(0) t "synthetic minima" w p lc 'gray' pt 1 ps 1.0,\
  "Omc12.dat" u ($1-2400000):(0) t "observed minima" w p lc 'green' pt 2 ps 2,\
  "chi2_TTV.dat" u ($1-2400000):($3*day):($5*day) t "O-C" w err lc 'blue' lw 1 pt 1 ps 0.5,\
  "<awk '!/^#/{ print $1,0.0; print $1,$3; print s; }' chi2_TTV.dat" u ($1-2400000):($2*day) t "residua" w l lc 'red' lw 3,\
  "<awk '($6>10.0)' chi2_TTV.dat" u ($1-2400000):($3*day) t "chi^2 > 10.0" w p pt 6 ps 2 lc 'red',\

q

  "Omc12.dat" u ($1-2400000):($3*day) w p t "Mayer et al., Fig. 9" lc 'gray'

