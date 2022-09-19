#!/usr/bin/gnuplot

set xl "lambda [nm]"
set yl "normalized response []"

set arrow from 5450,graph 0 rto 0,graph 1 nohead lt 0
set arrow from 5450-850/2.,graph 0 rto 0,graph 1 nohead lt 0
set arrow from 5450+850/2.,graph 0 rto 0,graph 1 nohead lt 0

lambda_eff = 5250.
band_eff = 3000.
set arrow from lambda_eff,graph 0 rto 0,graph 1 nohead lc 'gray'
set arrow from lambda_eff-band_eff/2,graph 0 rto 0,graph 1 nohead lc 'gray'
set arrow from lambda_eff+band_eff/2,graph 0 rto 0,graph 1 nohead lc 'gray'

p "most.ptf" u 1:2,\
  "johnson.V" u 1:2

pa -1

set term png small
set out "most.png"
rep

