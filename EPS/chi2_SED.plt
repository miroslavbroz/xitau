#!/usr/bin/gnuplot

set colors classic
set term post eps enh color dashed
set out "chi2_SED.eps"
set size 1.0,0.7

nm = 1.e-9

set xl "{/Symbol l}_{eff} [nm]"
set yl "magnitude [mag]"

set xr [300:30000]
set yr [:] reverse
set logscale x
set zeroaxis

tmp= 367; set label "U" at tmp,graph 1.03 center; set arrow from tmp,graph 0 rto 0,graph 1 nohead lt 0
tmp= 436; set label "B" at tmp,graph 1.03 center
tmp= 545; set label "V" at tmp,graph 1.03 center
tmp= 720; set label "R" at tmp,graph 1.03 center
tmp= 900; set label "I" at tmp,graph 1.03 center
tmp=1220; set label "J" at tmp,graph 1.03 center
tmp=1630; set label "H" at tmp,graph 1.03 center
tmp=2190; set label "K" at tmp,graph 1.03 center; set arrow from tmp,graph 0 rto 0,graph 1 nohead lt 0

set lmargin 8.0
set rmargin 0.2
set bmargin 3.2
set tmargin 1.2

p \
  "chi2_SED.dat" u ($1/nm):3 t "synthetic" w p pt 1 lt 8,\
  "Sed.dat" u ($1/nm):3   t "observed" w l   lt 3,\
  "Sed.dat" u ($1/nm):3:($2/2/nm):4 not  w xyerr lt 3 ps 0,\
  "chi2_SED.dat" u ($1/nm):3 t "residua" w l lt 1 lw 3

q


