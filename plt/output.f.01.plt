#!/usr/bin/env gnuplot

load "output.gnu"

set tit "s'', o'' vectors are misaligned..."
set xl "x"
set yl "y"
set zl "z"
set cbl "f [1]" offset 3,0

tmp=150000
#set xr [-tmp:tmp]
#set yr [-tmp:tmp]
#set zr [-tmp:tmp]
set cbr [0:]
set cbr [0:0.1]
#set nocolorbox

set view 0,0,1.5
set view equal xyz
set xyplane 0.0
set zeroaxis
set palette gray
set surface hidden3d
set pm3d depthorder
set hidden3d front
#set nocolorbox

#set xl tc 'white'
#set yl tc 'white'
#set zl tc 'white'
set cbl tc 'white'
#set xtics tc 'white'
#set ytics tc 'white'
#set ztics tc 'white'
set cbtics tc 'white'
#set border lc 'white'
set object rectangle from screen 0,0 to screen 1.01,1.01 behind fc 'black' fs solid noborder

#set arrow from 0,0,0 to s1__,s2__,s3__ front lc 'orange'
#set arrow from 0+0.01,0,0 to o1__+0.01,o2__,o3__ front lc 'blue'

tmp=300000

sp \
  "<./pm3d.awk output.node.01  output.face.01  output.f.01"  u ($1+ 0*tmp):2:3:5 w pm3d not,\

pa -1

set term png small size 1024,1024
set out "output.f.01.png"
rep

q



