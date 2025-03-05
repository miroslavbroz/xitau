#!/usr/bin/gnuplot

set colors classic
set term x11

nm = 1.e-9

set xl "lambda_eff [nm]"
set yl "UBV magnitude [mag]"

#set xr [200:30000]
set yr [:] reverse
set ytics 0.1
#set logscale x
set zeroaxis

tmp= 367; set label "U" at tmp,graph 1.01 center; set arrow from tmp,graph 0 rto 0,graph 1 nohead lt 0
tmp= 436; set label "B" at tmp,graph 1.01 center
tmp= 545; set label "V" at tmp,graph 1.01 center
tmp= 720; set label "R" at tmp,graph 1.01 center
tmp= 900; set label "I" at tmp,graph 1.01 center
tmp=1220; set label "J" at tmp,graph 1.01 center
tmp=1630; set label "H" at tmp,graph 1.01 center
tmp=2190; set label "K" at tmp,graph 1.01 center; set arrow from tmp,graph 0 rto 0,graph 1 nohead lt 0

p \
  "chi2_SED.dat" u ($1/nm):3 t "synthetic" w p pt 1 lc 'orange',\
  "Sed.dat" u ($1/nm):3   t "observed" w l   lc 'blue',\
  "Sed.dat" u ($1/nm):3:($2/2/nm):4 not  w xyerr ps 0 lc 'blue',\
  "chi2_SED.dat" u ($1/nm):3 t "residua" w l lw 3 lc 'red'
pa -1

set term png small
set out "chi2_SED.png"
rep

q


