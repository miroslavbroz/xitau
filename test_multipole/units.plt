#!/usr/bin/gnuplot

G = 6.67430e-11  # kg^-1 m^3 s^-2 
M_S = 1.989e30  # kg
day = 86400.
au = 1.49597870700e11  # m

GM_S = G*M_S

GM_S_DE405 = 0.2959122082855911e-03

print "GM_S = ", GM_S, " kg^-1 m^3 s^-2 = ", GM_S/(au**3/day**2), " au^3 day^-2"
print "GM_S = ", GM_S_DE405, " au^3 day^-2 (DE405)"

