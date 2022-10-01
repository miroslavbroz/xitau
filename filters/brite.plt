#!/usr/bin/gnuplot

ang = 0.1

set colors classic
set term x11

set xl "lambda [nm]"
set yl "normalized response []"

#set xr [200:15000]
#set yr [-0.1:1.1]
#set logscale x
set zeroaxis
set key spacing 0.7
set arrow from graph 0,first 0.5 rto graph 1,first 0 nohead lt 0

lambda_eff = 426.0
band_eff   =  61.0
set arrow from lambda_eff,graph 0 rto 0,graph 1 nohead lt 1 lc 'gray'
set arrow from lambda_eff-band_eff/2,graph 0 rto 0,graph 1 nohead lt 1 lc 'gray'
set arrow from lambda_eff+band_eff/2,graph 0 rto 0,graph 1 nohead lt 1 lc 'gray'

lambda_eff = 621.0
band_eff   = 145.0
set arrow from lambda_eff,graph 0 rto 0,graph 1 nohead lt 1 lc 'gray'
set arrow from lambda_eff-band_eff/2,graph 0 rto 0,graph 1 nohead lt 1 lc 'gray'
set arrow from lambda_eff+band_eff/2,graph 0 rto 0,graph 1 nohead lt 1 lc 'gray'

p \
  "brite_blue.ptf"  u ($1*ang):2 t "BRITE blue" w lp lc 'blue',\
  "brite_red.ptf"   u ($1*ang):2 t "BRITE red"  w lp lc 'red',\
  1 not lt 0
pa -1

set term png small
set out "brite.png"
rep

