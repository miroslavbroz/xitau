#!/usr/bin/gnuplot

set colors classic
set term post eps enh color dashed
set out "chi2_TTV.eps"
set size 1.0,0.7

#day = 1440.  # min
day = 1.  # day

set xl "JD {/Symbol -} 2400000"
set yl "ETV [day] wrt. N-body model" offset +0.5,0

tmp = 0.25*day  # day
set yr [-tmp:tmp]
set zeroaxis

load "T0.plt"
set arrow from T0-2400000,graph 0 to T0-2400000,graph 1 nohead lt 0

p "minima.dat" u ($1-2400000):(0) t "synthetic minima (primary and secondary)" w p lt 7 pt 1 ps 1.0,\
  "Omc12.dat" u ($1-2400000):(0) t "observed minima" w p lt 2 pt 2 ps 2,\
  "chi2_TTV.dat" u ($1-2400000):($3*day):($5*day) t "O-C" w err lt 1 lw 1 pt 1 ps 0.5,\
  "Omc12.dat" u ($1-2400000):($3*day) w p t "Mayer et al., Fig. 9" lc 'gray'

q

