#!/usr/bin/env gnuplot

set xl "lambda [nm]"
set yl "normalized response []"

lambda_eff = 7970.
band_eff = 4000.
set arrow from lambda_eff,graph 0 rto 0,graph 1 nohead lc 'gray'
set arrow from lambda_eff-band_eff/2,graph 0 rto 0,graph 1 nohead lc 'gray'
set arrow from lambda_eff+band_eff/2,graph 0 rto 0,graph 1 nohead lc 'gray'

p "tess.ptf" u 1:2,\
  1.0 w l lt 0

pa -1

set term png small
set out "tess.png"
rep

