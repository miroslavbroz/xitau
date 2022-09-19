#!/usr/bin/gnuplot

day = 86400.  # s
R_S = 6.957e8  # m
M_S = 1.989e30  # kg
G = 6.67430e-11  # kg^-1 m^3 s^-2

m1 = 1.0*M_S
R1 = 1.0*R_S
J2 = +2.e-7  # see multipole.in; J2 = -Clm(2,0)
Pr = 26.6*day

Omega0 = 2.*pi/Pr
n0 = sqrt(G*m1/R1**3)
Pk = 2.*pi/n0
k2 = J2/(Omega0/n0)**2

print "Pk = ", Pk/day, " d"
print "Pr = ", Pr/day, " d"
print "Omega0 = ", Omega0, " rad s^-1"
print "n0 = ", n0, " rad s^-1"
print "J2 = ", J2
print "k2 = ", k2

