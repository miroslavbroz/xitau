#!/usr/bin/gnuplot

cm = 1.e-2  # m
R_S = 6.957e8  # m
M_S = 1.989e30  # kg
G = 6.67430e-11  # kg^-1 m^3 s^-2

m1 = 17.8*M_S
m2 = 8.5*M_S
R1 = 8.0*R_S
R2 = 3.5*R_S

logg1 = log10(G*m1/R1**2/cm)
logg2 = log10(G*m2/R2**2/cm)

print "logg1 = ", logg1
print "logg2 = ", logg2

