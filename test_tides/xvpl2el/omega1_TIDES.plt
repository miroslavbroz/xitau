#!/usr/bin/gnuplot

# omega1_TIDES.plt
# Test pericentre precession due to tides (Fabrycky 2010).
# Miroslav Broz (miroslav.broz@email.cz), Jun 7th 2016

set term x11

AU = 1.49597870691e11  # m
M_S = 2.959139768995959e-04  # AU^3/day^2
M_J = 1.e-3*M_S
R_S = 6.995e8/AU
R_J = 7.1492e7/AU
G = 1.
day = 1.
yr = 365.25*day
Myr = 1.e6*yr

rad = 180./pi

k_L = 0.34
m_star = 1.27*M_S
m_p = 0.672*M_J
e = 0.
R_p = 1.3*R_J
P = 4.617136*day

# a^3/P^2 = G(m1+m2)/(4 pi^2)

a = (G*(m_star+m_p)/(4.*pi**2) * P**2)**(1./3.)
n = 2.*pi/P

domega_dt_T = 15./2.*n*k_L * m_star/m_p * (1.+3./2.*e**2 + 1./8.*e**4)/((1.-e**2)**5) * (R_p/a)**5
P_T = 2.*pi/(domega_dt_T)

print "m_p = ", m_p, " AU^3/day^2 = ", m_p/M_S, " M_S"
print "R_p = ", R_p, " AU = ", R_p/R_S, " R_S"
print "a = ", a, " AU"
print "n = ", n, " rad/day"
print "domega/dt_T = ", domega_dt_T, " rad/day"
print "P_T = ", P_T/day, " day = ", P_T/yr, " yr = ", P_T/Myr, " Myr"

########################################################################

f(x) = x > 180. ? x - 360. : x

set xl "time [yr]"
set yl "omega_1 [deg]"

set ytics format "%.1e"
set key left
set zeroaxis

p "xvpl2el.out" u ($1/yr):(f($7)) t "tides" w lp lt 1,\
  0.0 + domega_dt_T*(x*yr)*rad t "domega/dt_T" w l lt 7 lw 3
pa -1

set term png small
set out "omega1_TIDES.png"
rep

q


