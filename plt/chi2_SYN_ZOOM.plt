#!/usr/bin/gnuplot

set term x11

nm = 1.e-9  # m
ang = 1.e-10/nm  # nm
shift=0.25

set xl "lambda [nm]"
set yl "I_lambda [] (shifted by 1/4 dataset number)"

set xr [405:450]
set yr [12*shift:22*shift]

set ytics shift
set mytics 1
set grid ytics mytics
set zeroaxis
set bar 0.5

call "line.plt" "Halpha" 6563
call "line.plt" "Hbeta"  4861
call "line.plt" "Hgamma" 4341
call "line.plt" "Hdelta" 4102
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

p \
  "synthetic.dat" u ($2/nm):($3+shift*$4)   t "synthetic" w l lt 7,\
  "Spectra.dat"   u ($2/nm):($3+shift*$5):4 t "observed" w err lt 3 pt 1 ps 0,\
  "chi2_SYN.dat"  u ($2/nm):($3+shift*$5)   t "residua"  w l lt 1 lw 1,\
  "<awk '($NF+0>20)' chi2_SYN.dat" u ($2/nm):($3+shift*$5) t "chi^2 > 100" w p lt 1 pt 6 ps 1.5
pa -1

set term png small size 1280,800
set out "chi2_SYN_ZOOM.png"
rep

q


