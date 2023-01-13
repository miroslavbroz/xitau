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
  "AO1_1_ADAM.png" binary filetype=png with rgbimage,\
  "test_silh.silh3" u ($1/pixel_scale+127-0.5):($2/pixel_scale+127+0.5) w l,\

pa -1

q

