#!/usr/bin/gnuplot

set term x11
load "output.gnu"

set xl "{/Helvetica-Oblique x}" offset 0,4
set yl "{/Helvetica-Oblique y}"
set zl "{/Helvetica-Oblique z}" offset 1,0
set cbl "{/Helvetica-Oblique I}_{/Symbol l} [10^8 W m^{-2} sr^{-1} m^{-1}]" offset 0.5,0

#tmp=1.5e5
#set xr [-tmp:tmp]
#set yr [-tmp:tmp]
#set zr [-tmp:tmp]
set cbr [0:]

set xtics scale 0.5 offset 0,0.5
set cbtics offset -0.5,0
set colorbox user origin 0.38,0.5 size 0.02,0.35
set view 90,0,1.0
set view equal xyz
set xyplane 0.0
set zeroaxis
set palette gray
set surface hidden3d
set pm3d depthorder
set hidden3d front

scl=1.3e0
set arrow from scl*(0-0.01),0,0 to scl*(s1__-0.01),scl*s2__,scl*s3__ front lc 'orange'
set arrow from scl*(0+0.01),0,0 to scl*(o1__+0.01),scl*o2__,scl*o3__ front lc 'blue'
set label "{/Helvetica-Bold s} " at scl*(s1__-0.01),scl*s2__,scl*s3__ right
set label " {/Helvetica-Bold o}" at scl*(o1__+0.01),scl*o2__,scl*o3__ left

sp \
  "<./pm3d.awk output.node.01 output.face.01 output.I_lambda.01" u 1:2:3:($5/1.e8) w pm3d not,\
  "<./poly.awk output.poly5.01" u 4:5:6 w lp lw 1 lc 'green' not,\

#pa -1

set term post eps enh color solid "Helvetica" 18
set out "output.I_lambda.01.eps"
set size 0.525,1.0
set lmargin 0.0
set rmargin 0.0
set bmargin 0.0
set tmargin 0.0
rep

system("patch output.I_lambda.01.eps output.I_lambda.01.eps.diff")

q


