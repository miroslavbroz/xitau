#!/usr/bin/gnuplot

cm = 1.e-2  # m
day = 86400.  # s
R_S = 6.957e8  # m
M_S = 1.989e30  # kg
au = 149597870700.  # m
G = 6.67430e-11  # kg^-1 m^3 s^-2

R1 = 1.0  # m
R2 = 1.0  # m
a1 = 2.5  # m; as in lc_triangle/test_2spheres
P1 = 1.0*day
q = 0.5

#a1 = 1.*au
#P1 = 365.25*day
#R1 = 1.0*R_S
#R2 = 6.378e3  # m
#q = 3.e-6

mtot = (a1**3/P1**2) * 4.*pi**2/G
#m1+m2 = mtot
#m1(1+q) = mtot
m1 = mtot/(1.+q)
m2 = q*m1

V1 = 4./3.*pi*R1**3
V2 = 4./3.*pi*R2**3
rho1 = m1/V1
rho2 = m2/V2

logg1 = log10(G*m1/R1**2/cm)
logg2 = log10(G*m2/R2**2/cm)

print "P1 = ", P1, " s = ", P1/day, " day"
print "a1 = ", a1, " m = ", a1/au, " au"
print "mtot = ", mtot, " kg = ", mtot/M_S, " M_S"
print "q = ", m2/m1
print "m1 = ", m1, " kg = ", m1/M_S, " M_S"
print "m2 = ", m2, " kg = ", m2/M_S, " M_S"
print "V1 = ", V1, " m^3"
print "V2 = ", V2, " m^3"
print "rho1 = ", rho1, " kg m^-3"
print "rho2 = ", rho2, " kg m^-3"
print "logg1 = ", logg1
print "logg2 = ", logg2


