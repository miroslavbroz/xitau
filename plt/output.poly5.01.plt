#!/usr/bin/gnuplot

au = 1.49597870700e11
deg = pi/180.
arcsec = deg/3600.
mas = 1.e-3*arcsec

set term x11
load "output.gnu"

set xl "x"
set yl "y"
set zl "z"
set cbl "-"

#set xr [-0.5*tmp:5*tmp]
#set yr [-0.5*tmp:0.5*tmp]
set cbr [0:]

set view 0,0,1
set view equal xyz
set xyplane 0.0
set palette gray

scl=1
set arrow from scl*(0+0.00),0,0 to scl*(s1__+0.00),scl*s2__,scl*s3__ front lc 'orange'
set arrow from scl*(0+0.01),0,0 to scl*(o1__+0.01),scl*o2__,scl*o3__ front lc 'blue'

scl=arcsec*2.13832494981512*au

sp \
  "<./poly.awk output.poly5.01"  u 4:5:6 w l not,\
  "<./poly.awk output.poly5.01 | awk '($1==53) || (NF==0)'" u 4:5:6 w l lw 3 lc 'black',\

pa -1

set term png small size 2048,2048
set out "output.poly5.01.png"
rep

q
  "<./poly.awk output.poly5.10"  u ($4+1*tmp):5:6 w l not,\
  "<./poly.awk output.poly5.20"  u ($4+2*tmp):5:6 w l not,\
  "<./poly.awk output.poly5.30"  u ($4+3*tmp):5:6 w l not,\
  "<./poly.awk output.poly5.40"  u ($4+4*tmp):5:6 w l not,\
  "<./poly.awk output.poly5.50"  u ($4+5*tmp):5:6 w l not,\
  "<./poly.awk output.poly5.60"  u ($4+6*tmp):5:6 w l not,\
  "<./poly.awk output.poly5.70"  u ($4+7*tmp):5:6 w l not,\
  "<./poly.awk output.poly5.80"  u ($4+8*tmp):5:6 w l not,\
  "<./poly.awk output.poly5.90"  u ($4+9*tmp):5:6 w l not,\
  "<./poly.awk output.poly5.99"  u ($4+10*tmp):5:6 w l not,\
  "<./poly.awk output.poly5.110" u ($4+11*tmp):5:6 w l not,\
  "<./poly.awk output.poly5.120" u ($4+12*tmp):5:6 w l not,\
  "<./poly.awk output.poly5.130" u ($4+13*tmp):5:6 w l not,\

  "nodes0001.silh" u (scl*$1):(scl*$2):(0.0) w l lw 3,\
  "<./poly.awk output.poly5.99" u 4:5:6 w l not,\
  "<./poly.awk output.poly5.50" u 4:5:6 w lp not,\
  "<./poly.awk output.poly5.99" u 4:5:6 w lp not,\
  "<./poly.awk output.poly5.49 | awk '($1==88) || (NF==0)'" u 4:5:6 w l lw 3 lc 'black',\
  "<awk '(NR>1)' output.centre.49" u 2:3:4:1 w labels tc 'brown' not,\


