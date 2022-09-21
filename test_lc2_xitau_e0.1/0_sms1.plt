#!/usr/bin/gnuplot

set xl "phase"
set yl "sms1, sms2"

set arrow from graph 0,first 17.8 rto graph 1,first 0 nohead lt 0
set arrow from graph 0,first  8.5 rto graph 1,first 0 nohead lt 0

p "<awk '/gpha/{ gpha=$4; }/sms1/{ print gpha,$4; }' chi2.out" u 1:2 w lp,\
  "<awk '/gpha/{ gpha=$4; }/sms2/{ print gpha,$4; }' chi2.out" u 1:2 w lp,\

pa -1


