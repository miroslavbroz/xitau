#!/usr/bin/gnuplot

set colors classic
set term post eps enh color solid
set out "synthetic2.eps"
set size 1.0,0.6

nm = 1.e-9  # m
km = 1.e3  # m
c = 299792458.  # m/s
ang = 1.e-10/nm  # nm
shift = 1.0

set xl "{/Symbol l} [nm]"
set yl "{/Helvetica-Oblique I}_{/Symbol l} [] (shifted by dataset number)" offset 1,0

set xr [499:506.5]
set yr [7+0.85:7+1.05]
set zeroaxis
set bar 0.5
set key bottom

set arrow from graph 0,first 8 rto graph 1,first 0 nohead lt 0

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
#call "line2.plt" "-14"   (5016*(1-14*km/c))
call "line2.plt" "-257"  (5016*(1-257*km/c))  "red"
call "line2.plt" "+257"  (5016*(1+257*km/c))  "red"
call "line2.plt" "-199"  (5016*(1-199*km/c))  "green"
call "line2.plt" "+199"  (5016*(1+199*km/c))  "green"
call "line2.plt" "-51"   (5016*(1- 51*km/c))  "blue"
call "line2.plt" "+51"   (5016*(1+ 51*km/c))  "blue"
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

set lmargin 8.0
set rmargin 0.2
set bmargin 3.2
set tmargin 1.2

p \
  "<awk '($6==1) || (NF==0)' synthetic2.dat" u ($2/nm):(1+($3-1)*$4/$5+shift*($7-1)) t "1" w l lt 1,\
  "<awk '($6==2) || (NF==0)' synthetic2.dat" u ($2/nm):(1+($3-1)*$4/$5+shift*($7-1)) t "2" w l lt 2,\
  "<awk '($6==3) || (NF==0)' synthetic2.dat" u ($2/nm):(1+($3-1)*$4/$5+shift*($7-1)) t "3" w l lt 3,\
  "<awk '($6==4) || (NF==0)' synthetic2.dat" u ($2/nm):(1+($3-1)*$4/$5+shift*($7-1)) t "4" w l lt 4,\
  "Spectra.dat"   u ($2/nm):($3+shift*($5-1)):4 t "observed" w err lt 3 pt 1 ps 0

q


