#!/usr/bin/gnuplot

set xl "JD - 2400000"
set yl "lite [d]"
set zeroaxis

load "T0.plt"
set arrow from T0-2400000,graph 0 rto 0.0,graph 1 nohead lt 0

p \
  "<awk '($2==1)' lite.dat" u ($1-2400000):3 w l t "1",\
  "<awk '($2==2)' lite.dat" u ($1-2400000):3 w l t "2",\
  "<awk '($2==3)' lite.dat" u ($1-2400000):3 w l t "3",\
  "<awk '($2==4)' lite.dat" u ($1-2400000):3 w l t "4",\
  "<awk '($2==1)' lite2.dat" u ($1-2400000):3 w lp t ""

pa -1

set term png small size 2048,1024
set out "lite.png"
rep


