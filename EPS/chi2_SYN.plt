#!/usr/bin/gnuplot

set colors classic
set term post eps enh color solid
set out "chi2_SYN.eps"
set size 1.2,0.9

nm = 1.e-9  # m
shift = 0.25

set xl "{/Symbol l} [nm]"
set yl "{/Helvetica-Oblique I}_{/Symbol l} [] (shifted by 1/4 dataset number)" offset 1,0

set xr [390:550]
set yr [0.625:2.875]
set grid ytics mytics
set zeroaxis
set bar 0.1
set key samplen 1.0 width -2 outside

load "config.plt"

ang = 0.1
call "line.plt" "H{/Symbol a}" 6563
call "line.plt" "H{/Symbol b}" 4861
call "line.plt" "H{/Symbol g}" 4341
call "line.plt" "H{/Symbol d}" 4102
call "line.plt" "HeI"    4009
call "line.plt" "HeI"    4026
call "line.plt" "HeI"    4120
call "line.plt" "HeI"    4143
call "line.plt" "HeI"    4387
call "line.plt" "HeI"    4471
call "line.plt" "HeI"    4713
call "line.plt" "HeI"    4922
call "line.plt" "HeI"    5016
call "line.plt" "HeI"    5047
call "line.plt" "HeI"    5876
call "line.plt" "HeI"    6678
call "line.plt" "CII"    4267
call "line.plt" "CII"    6578
call "line.plt" "CII"    6582
call "line.plt" "MgII"   4481
call "line.plt" "SiII"   4128
call "line.plt" "SiII"   4130
call "line.plt" "SiII"   6347
call "line.plt" "SiII"   6371
call "line.plt" "NeI"    6402

set lmargin 7.0
set rmargin 13
set bmargin 3.2
set tmargin 1.5

p \
  "Spectra.dat"   u ($2/nm):($3+shift*($5-1)):4 t "observed" w err lt 3 pt 1 ps 0,\
  "synthetic.dat" u ($2/nm):($3+shift*($4-1))   t "synthetic" w l lc 'orange',\
  "chi2_SYN.dat"  u ($2/nm):($3+shift*($5-1))   t "residua"  w l lc 1 lw 1,\
  "<awk '($NF+0>100)' chi2_SYN.dat" u ($2/nm):($3+shift*($5-1)) t "{/Symbol c}^2 > 100" w p lt 1 pt 6 ps 0.5

q


