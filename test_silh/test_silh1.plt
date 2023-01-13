#!/usr/bin/gnuplot

set xl "u [arcsec]"
set yl "v [arcsec]"

#tmp=0.2
#set xr [-tmp:tmp]
#set yr [-tmp:tmp]
set size ratio -1
set zeroaxis

p \
  "<./face.awk nodes001.dat output.face" u 1:2 w l lc 'gray',\
  "test_silh.silh1" u 1:2 w lp ps 2,\
  "test_silh.silh2" u 1:2 w l  ps 2,\
  "test_silh.silh3" u 1:2 w l  ps 2,\

pa -1

q

  "<awk '(NR<2){ print $2,$3; print $2+$4,$3+$5; print s; }' test_silh.out" u 1:2 w lp lw 2 lc 'orange',\
  "<awk '(NR<2){ print $6,$7; print $6+$8,$7+$9; print s; }' test_silh.out" u 1:2 w lp lw 1 lc 'blue',\
  "<awk '(NR<2){ print $10,$11; }' test_silh.out" u 1:2 w p lw 2 lc 'red',\

