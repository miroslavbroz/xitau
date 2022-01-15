#!/usr/bin/gnuplot

h = 3600.
km = 1.e3

P = 5.385*h

scale = 1.0

omega = 2*pi/P
Ueff(x,y,U) = U - 1./2.*omega**2*(x**2+y**2)

########################################################################

set term qt

set xl "x [km]"
set yl "y [km]"
set zl "U [J kg^-1]"

tmp=250
set xr [-tmp:tmp]
set yr [-tmp:tmp]
z1=-10000
z2=0
set zr [z1:z2]
set ztics 1000
set xyplane 0.0
set view equal xy
set view 0,0
set nokey
set contour base; set nosurface; pa=0
#set contour surface; set surface; pa=-1  # dbg

L1 = -2974; set cntrparam levels discrete L1  # right
L2 = -2893; set cntrparam levels discrete L2  # left
L3 = -2155; set cntrparam levels discrete L3  # up
L4 = -2152; set cntrparam levels discrete L4  # down
set cntrparam levels increment -500,-250,-10000

set table "gravity3a.dat"
sp "gravity3.out" u 1:2:(Ueff($1*km,$2*km,$4)) w l lc 'gray'; pa pa
unset table

set cntrparam levels discrete L1, L2, L3, L4

set table "gravity3b.dat"
rep
unset table

set term post eps color solid enh "Helvetica,18"
set size 0.75,1.0
set out "gravity3__5.385.eps"

p \
  "gravity3a.dat" u 1:2 w l lc 'gray',\
  "gravity3b.dat" u 1:2 w l lc 'black',\
  "<./face.awk tri_file_octdec_3.1.node tri_file_octdec_3.1.face" u ($1*scale):($2*scale) w l lc 'red' lw 0.5

q


