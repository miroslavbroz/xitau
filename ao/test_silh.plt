#!/usr/bin/gnuplot

minl = `awk '/minl = /{ print $3; }' test_silh.out`

set xl "l [pxl]"
set yl "f, f' [adu, adu/pxl]"

set zeroaxis
set arrow from minl,graph 0 rto 0,graph 1 nohead lt 0 front

p \
  "<awk '/f\\(/'  test_silh.out" u 2:5 w lp,\
  "<awk '/f_\\(/' test_silh.out" u 2:5 w lp,\
  1000.0 w l lt 0

pa -1

q

  "<awk '/f\\(/'  test_silh.out91" u 2:5 w lp,\
  "<awk '/f_\\(/' test_silh.out91" u 2:5 w lp,\
  "test_silh.silh2" u 2:3 w lp ps 2,\
  "<./face.awk nodes001.dat output.face" u 1:2 w l lc 'gray',\
  "<awk '(NR<2){ print $2,$3; print $2+$4,$3+$5; print s; }' test_silh.out" u 1:2 w lp lw 2 lc 'orange',\
  "<awk '(NR<2){ print $6,$7; print $6+$8,$7+$9; print s; }' test_silh.out" u 1:2 w lp lw 1 lc 'blue',\
  "<awk '(NR<2){ print $10,$11; }' test_silh.out" u 1:2 w p lw 2 lc 'red',\

