#!/usr/bin/gnuplot

set term post eps enh color dashed rounded
set out "chi2_SYN_ZOOM.eps"
set size 0.8,0.7

nm = 1.e-9  # m
shift = 0.25

set xl "{/Symbol l} [nm]"
set yl "{/Helvetica-Oblique I}_{/Symbol l} (shifted by 1/4 dataset number)" offset 1,0

set xr [440:455.85]
tmp = 25
set yr [(25-tmp)*shift+0.05:(29-tmp)*shift+0.1]

set ytics shift
set mytics 1
set grid ytics mytics
set zeroaxis
set bar 0.1
set key samplen 1.0 spacing 1.2

load "config.plt"

p \
  "<./cnv.awk Spectra.dat" u ($2/nm):($3+shift*($5-1-tmp)-$4):($3+shift*($5-1-tmp)+$4) t "{/=10 uncertainty}" w filledcurves close lc 9,\
  "<./cnv.awk Spectra.dat" u ($2/nm):($3+shift*($5-1-tmp)) t "observed"  w l lt 1 lc 3 lw 2,\
  "synthetic.dat"          u ($2/nm):($3+shift*($4-1-tmp)) t "synthetic" w l lt 1 lc 8 lw 1

q

  "chi2_SYN.dat"  u ($2/nm):($3+shift*($5-1))   t "residua"   w l   lt 1 lc 1 lw 1,\

