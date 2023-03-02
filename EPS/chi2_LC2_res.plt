#!/usr/bin/gnuplot

load "T0.plt"

JD0 = 2436258.0000000000
P = x_param4
P = 0.17284167583333299

f1(x) = x > 0.0 ? x : x+1.0
frac(x) = f1(x-int(x))
phase(jd) = frac((jd-JD0)/P)

set colors classic
set term x11

band = 7
band = 54
shift = 0.0

set xl "phase [1]"
set yl "{/Helvetica-Oblique O} {/Symbol -} {/Helvetica-Oblique C} [mag]" offset 2,0
set cbl "JD {/Symbol -} 2400000" offset -1,0

load "T0.plt"

set yr [0.03:-0.03]
set ytics offset 0.3,0 font "Helvetica,16"
set tics front
set zeroaxis
set grid front
set cbr [2459546.6921026399-2400000:2459548.7716091899-2400000]
set cbtics 0.5 offset -1,0 font "Helvetica,14"
unset colorbox
set palette defined (\
  0.0 "blue",\
  0.8 "#0080ff",\
  1.0 "cyan" \
  )
set key right top samplen 2.0 spacing 1.2 font "Helvetica,14"

p \
  "<awk '(NF==0){ i=0; }(NF>0){ i++; }(i==1){ v=$2; }(i==2){ print $1,$2-v,$3; }' chi2_LC2.dat" u (phase($1)):2:3:($1-2400000) w err pt 1 ps 0.5 lc palette z t "residua",\

#pa -1

set term post eps enh color solid "Helvetica" 18
set out "chi2_LC2_res.eps"
set size 0.85,0.4
set lmargin 6.5
set rmargin 0.6
set bmargin 3.0
set tmargin 0.6
rep

q


