#!/usr/bin/gnuplot

cm = 1.e-2  # m
G = 6.67430e-11  # kg^-1 m^3 s^-2, from 2018 CODATA

GM_S = 0.2959122082855911e-03  # au^3/day^2, from JPLEPH DE405
AU = 1.49597870700e11  # m, from IAU 2012
day = 86400.  # s
M_S = GM_S*AU**3/day**2/G

#M_S = 1.989e30  # kg
M_E = 5.792e24  # kg
R_S = 6.957e8  # m
R_E = 6.378e6  # m

print "M_S = ", M_S, " kg = ", M_S/M_S, " M_S"
print "M_E = ", M_E, " kg = ", M_E/M_S, " M_S"
print "R_S = ", R_S/R_S, " R_S"
print "R_E = ", R_E/R_S, " R_S"

g_S = G*M_S/R_S**2
g_E = G*M_E/R_E**2

print "logg_S = ", log10(g_S/cm), " [cgs]"
print "logg_E = ", log10(g_E/cm), " [cgs]"


