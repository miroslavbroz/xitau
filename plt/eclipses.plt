#!/usr/bin/env gnuplot

cm = 1.e-2  # m
G = 6.67430e-11  # kg^-1 m^3 s^-2
M_S = 1.989e30  # kg
R_S = 6.957e8  # m
au = 1.496e11 # m

load "T0.plt"

R1 = 6.4310049232306090*R_S
R2 = 4.6497487646759117*R_S

print "R1 = ", R1/R_S, " R_S"
print "R2 = ", R2/R_S, " R_S"

set xl "x [au]"
set yl "y [au]"

set zeroaxis
set size ratio -1
set palette rgbformulae 33,13,10

p \
  "<awk '($2==-2) && (i<=32){ i++; print; }' out_JDATE_heliocentric.dat" u 3:4 w lp pt 2 lc 'green' t "2",\
  sprintf("<echo \"0 0 0 %f %f 0 0\" | ./ellipses.awk", R1/au, R1/au) w l dt 2 lc 'blue' not,\
  sprintf("<echo \"0 %f 0 %f %f 0 0\" | ./ellipses.awk", -(R1+R2)/au, R2/au, R2/au) w l dt 2 lc 'blue' not,\
  "<awk '!/^ #/ && (NR<=6){ print 0,0,0,$5,$5,0,0; }' eclipses.dat | ./ellipses.awk" w l lw 0.5 lc 'gray' t "d_{max}",\
  "<awk '!/^ #/ && (NR<=6){ print 0,0,0,$6,$6,0,0; }' eclipses.dat | ./ellipses.awk" w l lc 'black' t "d",\
  "<awk '!/^ #/ && (NR<=6){ i++; print $1,$2,i; print $3,$4,i; print s; }' eclipses.dat" u 1:2:3 w lp pt 1 lw 3 lc palette z t "eclipses",\

pa -1

set term png small
set out "eclipses.png"
rep

