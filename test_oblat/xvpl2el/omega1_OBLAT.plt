#!/usr/bin/gnuplot

# omega1_OBLAT.plt
# Test pericentre precession due to oblateness (Fabrycky 2010).
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

k_L_star = 0.34
m_star = 1.27*M_S
m_p = 0.672*M_J
e = 0.
R_star = 1.0*R_S
P = 4.617136*day
P_star = 2.*day

# a^3/P^2 = G(m1+m2)/(4 pi^2)

a = (G*(m_star+m_p)/(4.*pi**2) * P**2)**(1./3.)
n = 2.*pi/P
Omega_star = 2.*pi/P_star

domega_dt_R = n*k_L_star/2. * (1.+m_p/m_star)/(1.-e**2)**2 * (Omega_star/n)**2 * (R_star/a)**5
P_R = 2.*pi/(domega_dt_R)

print "m_p = ", m_p, " AU^3/day^2 = ", m_p/M_S, " M_S"
print "a = ", a, " AU"
print "n = ", n, " rad/day"
print "Omega_star = ", Omega_star, " rad/day"
print "domega/dt_R = ", domega_dt_R, " rad/day"
print "P_R = ", P_R/day, " day = ", P_R/yr, " yr = ", P_R/Myr, " Myr"

########################################################################

f(x) = x > 180. ? x - 360. : x

set xl "time [yr]"
set yl "omega_1 [deg]"

set ytics format "%.1e"
set key left
set zeroaxis

p \
  "../../test_tides/xvpl2el/xvpl2el.out" u ($1/yr):(f($7)) t "tides" w lp lt 1,\
  "xvpl2el.out" u ($1/yr):(f($7)) t "oblateness" w lp lt 3,\
  0.0 + domega_dt_R*(x*yr)*rad t "domega/dt_R" w l lt 7 lw 3
pa -1

set term png small
set out "omega1_OBLAT.png"
rep

q


