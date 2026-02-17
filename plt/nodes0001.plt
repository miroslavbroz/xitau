#!/usr/bin/env gnuplot

pixel_scale = 3.6e-3  # arcsec/pxl
pixel_scale = 1.2e-3  # arcsec/pxl
w = 256
h = 256

#c  =   -1.6242093805569224        1.1517621045560109       pxl
#c_ =    1.6127382815832165E-002  -6.0601123568080123E-003  pxl
c1 = -1.6242093805569224
c2 = 1.1517621045560109
c1 = c1 + -2.0832515513309748E-002
c2 = c2 +  2.8088874447172148E-003
c1 = c1 + 0.5
c2 = c2 - 0.5

set tit "AO image may not be centered! (cf. (c1,c2))"
set xl "u, -RA [arcsec]"
set yl "v, +DE [arcsec]"
set zl "w, away from o. [arcsec]"

tmp=0.1
set xr [-tmp:tmp]
set yr [-tmp:tmp]
set size ratio -1
set zeroaxis

do for [i=-1:1:1] {
  set arrow from first i*pixel_scale,graph 0 rto first 0,graph 1 nohead lc 'green' front
  set arrow from graph 0,first i*pixel_scale rto graph 1,first 0 nohead lc 'green' front
}

p \
  "AO1_1.png" binary filetype=png origin=((-0.5*w+0.5-c1)*pixel_scale,(-0.5*h+0.5-c2)*pixel_scale) dx=pixel_scale dy=pixel_scale with rgbimage,\
  "nodes0001.silh"                        u 1:2 w l lc 'orange',\
  "nodes0001.silh_"                       u 1:2 w l lc 'blue',\
  "<awk '($6==1) || (NF==0)' chi2_AO.dat" u 2:3 w l lc 'red',\
  "<awk '{ print 0,0; print $0; print s; }' nodes0001.silh_" u 1:2 w l lc 'gray',\

pa -1

set term png small size 1024,1024
set out "nodes0001.png"
rep

q



