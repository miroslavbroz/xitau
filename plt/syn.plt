#!/usr/bin/gnuplot

set colors classic
set term x11

nm = 1.e-9  # m
ang = 1.e-10  # m
shift = 0.5

set xl "lambda [nm]"
set yl "I_lambda [] (shifted by 1/4 dataset number)"

set ytics shift
set grid ytics

p "1.syn" u ($1*ang/nm):($2+0*shift) t "1" w l,\
  "2.syn" u ($1*ang/nm):($2+1*shift) t "2" w l,\
  "3.syn" u ($1*ang/nm):($2+2*shift) t "3" w l,\
  "4.syn" u ($1*ang/nm):($2+3*shift) t "4" w l
pa -1

set term png small
set out "syn.png"
rep


