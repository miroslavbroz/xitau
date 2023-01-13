#!/usr/bin/gnuplot

pixel_scale = 3.6e-3  # arcsec/pxl
pixel_scale = 1.2e-3  # arcsec/pxl

set xl "u [pxl]"
set yl "v [pxl]"

#tmp=0.2
#set xr [-tmp:tmp]
#set yr [-tmp:tmp]
set size ratio -1
set zeroaxis
set arrow from first 127,graph 0 rto first 0,graph 1 nohead lc 'green' front
set arrow from graph 0,first 127 rto graph 1,first 0 nohead lc 'green' front

p \
  "AO1_1.png" binary filetype=png with rgbimage,\
  "test_silh.silh2" u ($1/pixel_scale+127):($2/pixel_scale+127) w l,\

pa -1

q

  "<echo 127 127" w p pt 1 ps 2
  "<awk '(NR<2){ print $2,$3; print $2+$4,$3+$5; print s; }' test_silh.out" u 1:2 w lp lw 2 lc 'orange',\
  "<awk '(NR<2){ print $6,$7; print $6+$8,$7+$9; print s; }' test_silh.out" u 1:2 w lp lw 1 lc 'blue',\
  "<awk '(NR<2){ print $10,$11; }' test_silh.out" u 1:2 w p lw 2 lc 'red',\

  "<./face.awk nodes001.dat output.face" u 1:2 w l lc 'gray',\
  "test_silh.silh1" u 2:3 w lp ps 2,\

