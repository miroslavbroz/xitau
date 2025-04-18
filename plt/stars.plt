#!/usr/bin/gnuplot

deg = pi/180.  # rad
day = 86400.  # s
cm = 1.e-2  # m
au = 1.49597870700e11  # m, from IAU 2012
R_S = 6.957e8  # m, from IAU 2015
GM_S = 0.2959122082855911e-03*au**3/day**2  # AU^3/day^2, from JPLEPH DE405
G = 6.67430e-11  # m^3 kg^-1 s^-2, from 2018 CODATA
M_S = GM_S/G

load "T0.plt"

# definitions:
#q2 = m2/m1
#q3 = m3/(m1+m2)
#q4 = m4/(m1+m2+m3)

msum = x_param1*M_S
q2 = x_param2
q3 = x_param3
q4 = x_param4
m1 = msum/((1.+q2)*(1.+q3)*(1.+q4))
m2 = q2*m1
m3 = q3*(m1+m2)
m4 = q4*(m1+m2+m3)

P1 = x_param5*day
e1 = 10.0**x_param6
i1 = x_param7*deg
g1 = 10.**x_param27*cm
g2 = 10.**x_param28*cm

R1 = sqrt(G*m1/g1)
R2 = sqrt(G*m2/g2)
a1 = (G*(m1+m2)/(4.*pi**2)*P1**2)**(1./3.)
q1 = a1*(1.-e1)
#L1 = 0.57516403411249928*a1  # test_roche_q0.487

# use q1 instead of a1 if e1 > 0.0!!!

print "m1 = ", m1/M_S, " M_S"
print "m2 = ", m2/M_S, " M_S"
print "logg1 = ", log10(g1/cm)
print "logg2 = ", log10(g2/cm)
print "a1 = ", a1/au, " au = ", a1/R_S, " R_S"
print "R1 = ", R1/R_S, " R_S = ", R1/a1, " a1"
print "R2 = ", R2/R_S, " R_S = ", R2/a1, " a1"
#print "L1 = ", L1/R_S, " R_S = ", L1/a1, " a1"

set term x11

set xl "x [a = 1 units]" offset -1,0
set yl "y"
set zl "z"
set cbl "T [K]" offset 1,0

tmp=0.1
set xr [-tmp:1+tmp]
set yr [-tmp-0.5:tmp+0.5]
set zr [-tmp-0.5:tmp+0.5]
c1=3000.
c2=40000.
set cbr [c1:c2]

set xyplane 0
set nokey
set grid
set zeroaxis
set view i1/deg+180,90,1.33,1.33
set view 0,90
set view equal xyz
set colorbox user origin 0.02,0.2

set style line 1 lt 1 lw 3
set style line 2 lt 7 lw 3

set palette defined (\
  ( 3000.-c2)/(c2-c1) "orange",\
  (10000.-c2)/(c2-c1) "#eeeeee",\
  (30000.-c2)/(c2-c1) "#6666ff",\
  (40000.-c2)/(c2-c1) "magenta"\
  )

set parametric
set urange [0:2.*pi]
set vrange [-pi/2.:pi/2.]
fx(r,u,v) = r*cos(u)*cos(v)
fy(r,u,v) = r*sin(u)*cos(v)
fz(r,u,v) = r*sin(v)

sp \
  "star1.dat" u 1:2:3:8         w d ls 1 lc palette z,\
  "star1.dat" u 1:(-$2):(+$3):8 w d ls 1 lc palette z,\
  "star1.dat" u 1:(+$2):(-$3):8 w d ls 1 lc palette z,\
  "star1.dat" u 1:(-$2):(-$3):8 w d ls 1 lc palette z,\
  "star2.dat" u 1:2:3:8         w d ls 2 lc palette z,\
  "star2.dat" u 1:(-$2):(+$3):8 w d ls 2 lc palette z,\
  "star2.dat" u 1:(+$2):(-$3):8 w d ls 2 lc palette z,\
  "star2.dat" u 1:(-$2):(-$3):8 w d ls 2 lc palette z,\
  fx(R1/a1,u,v)+0.0, fy(R1/a1,u,v), fz(R1/a1,u,v) lc 'green',\
  fx(R2/a1,u,v)+q1/a1, fy(R2/a1,u,v), fz(R2/a1,u,v) lc 'green'

pa -1

set term png small size 1024,1024
set out "stars.png"
rep

q

  "<echo 0 0 0" u (L1/a1):(0):(0) w p pt 1 ps 2 lc 'black'

