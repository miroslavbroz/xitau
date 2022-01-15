#!/usr/bin/gnuplot

eps_J2000 = (23.+26./60.+21.448/3600.)
eps = eps_J2000

set xl "l [deg]"
set yl "b [deg]"
set cbl "zeta [deg]"

set xr [0:360]
set yr [0:90]
set zr [-180:180]
set cbr [-180:180]

set xtics 90
set ytics 30
set ztics 30
set cbtics 30
set zeroaxis
set palette defined (0.0 '#ff0000', 0.5 '#ffffff', 1.0 '#0000ff' )
set xyplane 0.0
set grid xtics front

#set arrow from graph 0,first 90-eps rto graph 1,first 0 nohead lt 0 front
#set arrow from graph 0,first -eps rto graph 1,first 0 nohead lt 0 front

set contour base; set nosurface; pa=0
#set contour surface; set surface; pa=-1  # dbg

set cntrparam levels discrete -90.0,-eps,0.0,eps,90.0

set table "test_zeta.tmp"
sp "test_zeta.out" u 1:2:3 w l lc 'gray'; pa pa
unset table

set surface
unset contour
set pm3d map

sp \
  "test_zeta.out" u 1:2:3 w pm3d,\
  "test_zeta.tmp2" u 1:2:(0.0) w l lw 1 lc 'green'
pa -1

set term png small
set out "test_zeta.png"
rep

q



