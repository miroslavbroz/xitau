#!/usr/bin/env gnuplot

set xl "lambda [nm]"
set yl "normalized response []"

lambda_eff = 5250.
band_eff = 3000.
set arrow from lambda_eff,graph 0 rto 0,graph 1 nohead lc 'gray'
set arrow from lambda_eff-band_eff/2,graph 0 rto 0,graph 1 nohead lc 'gray'
set arrow from lambda_eff+band_eff/2,graph 0 rto 0,graph 1 nohead lc 'gray'

p "most.ptf" u 1:2 w l,\
  "most.ptf" u 1:($2/0.738156) w l,\
  1.0 w l lt 0

pa -1

set term png small
set out "most.png"
rep

