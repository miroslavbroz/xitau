#!/usr/bin/gnuplot

cm = 1.e-2  # m
G = 6.67430e-11  # kg^-1 m^3 s^-2
M_S = 1.989e30  # kg
R_S = 6.957e8  # m
au = 1.496e11 # m

load "T0.plt"

m1 = x_param1*M_S
m2 = x_param2*M_S
m3 = x_param3*M_S
g1 = 10**x_param19*cm
g2 = 10**x_param20*cm
g3 = 10**x_param21*cm
R1 = sqrt(G*m1/g1)
R2 = sqrt(G*m2/g2)
R3 = sqrt(G*m3/g3)

print "R1 = ", R1/R_S, " R_S"
print "R2 = ", R2/R_S, " R_S"
print "R3 = ", R3/R_S, " R_S"

set xl "x [au]"
set yl "y [au]"

set zeroaxis
set size ratio -1
set palette rgbformulae 33,13,10

p \
  "<awk '($2==-2) && (i<30){ i++; print; }' out_JDATE_heliocentric.dat" u 3:4 w lp pt 2 lc 'green' t "2",\
  sprintf("<echo \"0 0 0 %f %f 0 0\" | ./ellipses.awk", R1/au, R1/au) w l dt 2 lc 'blue' not,\
  sprintf("<echo \"0 %f 0 %f %f 0 0\" | ./ellipses.awk", -(R1+R2)/au, R2/au, R2/au) w l dt 2 lc 'blue' not,\
  "<awk '!/^ #/ && (NR<15){ print 0,0,0,$5,$5,0,0; }' eclipses.dat | ./ellipses.awk" w l lw 0.5 lc 'gray' t "d_{max}",\
  "<awk '!/^ #/ && (NR<15){ print 0,0,0,$6,$6,0,0; }' eclipses.dat | ./ellipses.awk" w l lc 'black' t "d",\
  "<awk '!/^ #/ && (NR<15){ i++; print $1,$2,i; print $3,$4,i; print s; }' eclipses.dat" u 1:2:3 w lp pt 1 lw 2 lc palette z t "eclipses",\

pa -1

set term png small
set out "eclipses.png"
rep

