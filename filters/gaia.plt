#!/usr/bin/gnuplot

set term x11

set xl "lambda [nm]"
set yl "f [1]"

set yr [0:1]
set zeroaxis

#lambda_eff = 623.06  # pivot
#lambda_eff = 640.50
lambda_eff = 639.02

#band_eff = 455.0
band_eff = 317.32

set arrow from lambda_eff,graph 0 rto 0,graph 1 nohead lt 0
set arrow from lambda_eff+band_eff/2,graph 0 rto 0,graph 1 nohead lt 0
set arrow from lambda_eff-band_eff/2,graph 0 rto 0,graph 1 nohead lt 0
set arrow from graph 0,first 0.5 rto graph 1,first 0.0 nohead lt 0

p \
  "<awk '($2<99.)' gaia.G" u 1:2 w lp,\

pa -1

q

  "<awk '($2<99.)' gaia.G" u 1:($2/0.72) w lp,\

