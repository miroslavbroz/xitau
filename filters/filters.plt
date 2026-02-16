#!/usr/bin/env gnuplot

ang = 0.1

set colors classic

set xl "lambda [nm]"
set yl "normalized response []"

set xr [200:15000]
set yr [-0.1:1.1]
#set logscale x
set zeroaxis
set nokey
set tmargin 2.0

# Reference: http://hea.iki.rssi.ru/AZT22/RUS/eaa_bessel.pdf
# Reference: https://en.wikipedia.org/wiki/Photometric_system

x=  367; y=  66; set label "U"  at x,graph 1.02 center; set arrow from x,graph 0 rto 0,graph 1 nohead lt 0; set arrow from x-y/2.,0.500 rto y,graph 0 heads lt  4
x=  436; y=  94; set label "B"  at x,graph 1.02 center; set arrow from x,graph 0 rto 0,graph 1 nohead lt 0; set arrow from x-y/2.,0.501 rto y,graph 0 heads lt  3
x=  545; y=  88; set label "V"  at x,graph 1.02 center; set arrow from x,graph 0 rto 0,graph 1 nohead lt 0; set arrow from x-y/2.,0.502 rto y,graph 0 heads lt  2
x=  638; y= 160; set label "R"  at x,graph 1.02 center; set arrow from x,graph 0 rto 0,graph 1 nohead lt 0; set arrow from x-y/2.,0.503 rto y,graph 0 heads lt  1
x=  720; y= 220; set label "Rc" at x,graph 1.02 center; set arrow from x,graph 0 rto 0,graph 1 nohead lt 0; set arrow from x-y/2.,0.504 rto y,graph 0 heads lt  1
x=  797; y= 149; set label "I"  at x,graph 1.02 center; set arrow from x,graph 0 rto 0,graph 1 nohead lt 0; set arrow from x-y/2.,0.505 rto y,graph 0 heads lt  5
x=  900; y= 240; set label "Ic" at x,graph 1.02 center; set arrow from x,graph 0 rto 0,graph 1 nohead lt 0; set arrow from x-y/2.,0.506 rto y,graph 0 heads lt  5
x= 1220; y= 213; set label "J"  at x,graph 1.02 center; set arrow from x,graph 0 rto 0,graph 1 nohead lt 0; set arrow from x-y/2.,0.507 rto y,graph 0 heads lt  6
x= 1630; y= 307; set label "H"  at x,graph 1.02 center; set arrow from x,graph 0 rto 0,graph 1 nohead lt 0; set arrow from x-y/2.,0.508 rto y,graph 0 heads lt  7
x= 2190; y= 390; set label "K"  at x,graph 1.02 center; set arrow from x,graph 0 rto 0,graph 1 nohead lt 0; set arrow from x-y/2.,0.509 rto y,graph 0 heads lt  8
x= 3450; y= 472; set label "L"  at x,graph 1.02 center; set arrow from x,graph 0 rto 0,graph 1 nohead lt 0; set arrow from x-y/2.,0.510 rto y,graph 0 heads lt  9
x= 4750; y= 460; set label "M"  at x,graph 1.02 center; set arrow from x,graph 0 rto 0,graph 1 nohead lt 0; set arrow from x-y/2.,0.511 rto y,graph 0 heads lt 10
x=10500; y=2500; set label "N"  at x,graph 1.02 center; set arrow from x,graph 0 rto 0,graph 1 nohead lt 0; set arrow from x-y/2.,0.512 rto y,graph 0 heads lt 11

p \
  "johnson.U" u ($1*ang):2 t "U"  w lp lt  4,\
  "johnson.B" u ($1*ang):2 t "B"  w lp lt  3,\
  "johnson.V" u ($1*ang):2 t "V"  w lp lt  2,\
  "johnson.R" u ($1*ang):2 t "R"  w lp lt  1,\
  "cousins.R" u ($1*ang):2 t "Rc" w l  lt  1 dt 2,\
  "johnson.I" u ($1*ang):2 t "I"  w lp lt  5,\
  "cousins.I" u ($1*ang):2 t "Ic" w l  lt  5 dt 2,\
  "johnson.J" u ($1*ang):2 t "J"  w lp lt  6,\
  "johnson.H" u ($1*ang):2 t "H"  w lp lt  7,\
  "johnson.K" u ($1*ang):2 t "K"  w lp lt  8,\
  "johnson.L" u ($1*ang):2 t "L"  w lp lt  9,\
  "johnson.M" u ($1*ang):2 t "M"  w lp lt 10,\
  "johnson.N" u ($1*ang):2 t "N"  w lp lt 11,\
  1 not lt 0
pa -1

set term png small
set out "filters.png"
rep


