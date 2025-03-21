#!/usr/bin/gnuplot

load "T0.plt"

JD0 = T0+(2458773.2054269998-2458773.1944269901)+5.7755183314369954E-003
P = x_param3

f1(x) = x > 0.0 ? x : x+1.0
frac(x) = f1(x-int(x))
phase(jd) = frac((jd-JD0)/P)

set colors classic
set term x11

band = 7
shift = 0.0

set xl "phase [1]"
set yl "{/Helvetica-Oblique V} [mag]" offset 1,0

set xr [-0.02:1.02]
set yr [0.035:-0.005]
set ytics 0.01
set zeroaxis
set key at 0.4,0.5 samplen 2 spacing 1.5
set mouse format "%.6f"

set arrow from 0.0,graph 0 rto 0,graph 1 nohead lt 0
set arrow from 0.5,graph 0 rto 0,graph 1 nohead lt 0
set arrow from 1.0,graph 0 rto 0,graph 1 nohead lt 0

set lmargin 7.5
set rmargin 0.2
set bmargin 3.0
set tmargin 0.6

p \
  "<awk '($1<2458773.1886517801+0.017)' lightcurve2.dat"                                   u (phase($1)):($2-31.217535366176389) w l lw 2 dt 1 lc 'cyan' t "42 nodes",\
  "<awk '($1>2458773.1886517801+0.017)' lightcurve2.dat"                                   u (phase($1)):($2-31.217535366176389) w l lw 2 dt 1 lc 'cyan' not,\

#pa -1

set term post eps enh color solid "Helvetica" 18
set out "chi2_LC2_PHASE.eps"
set size 0.8,0.7
rep

q


