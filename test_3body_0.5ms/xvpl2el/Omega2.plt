#!/usr/bin/gnuplot

deg = pi/180.  # rad
day = 86400.  # s
yr = 365.25*day
cm = 1.e-2  # m
au = 1.496e11  # m
R_S = 6.957e8  # m
M_S = 1.989e30  # kg
G = 6.67430e-11  # kg^-1 m^3 s^-2

load "T0.plt"

m1 = x_param1*M_S
m2 = x_param2*M_S
m3 = x_param3*M_S
P1 = x_param4*day
P2 = x_param10*day
e2 = x_param11
i2 = x_param12*deg

a1 = (G*(m1+m2)/(4.*pi**2)*P1**2)**(1./3.)
a2 = (G*(m1+m2+m3)/(4.*pi**2)*P2**2)**(1./3.)

#J2 = 1./2. * (a1/a2)**2 * m1*m2/(m1+m2)**2  # Broz etal. (2010); ERROR, (a/r)^2 term in J2!
J2 = 1./2. * (a1/a1)**2 * m1*m2/(m1+m2)**2  # perturbation radius r = a1, reference radius R = a1 => factor (a1/a1)**2 = 1.0

n2 = 2.*pi/P2
eta2 = sqrt(1.-e2**2)
dotOmega2 = -3./2.*n2*J2*(a1/a2)**2*cos(i2)/eta2**4  # standard coefficients; Bertotti etal. (2003), p. 332

print "a1 = ", a1/au, " au"
print "a2 = ", a2/au, " au"
print "n2 = ", n2, " rad s^-1"
print "J2 = ", J2, "  <-- cf. 2.e-7"
print "eta2^4 = ", eta2**4
print "cos i2 = ", cos(i2)
print "dotOmega2 = ", dotOmega2, " rad s^-1"

f(x) = x > 180. ? x - 360. : x

set xl "time [yr]"
set yl "Omega_1 [deg]"

set zeroaxis
set key left

load "T0.plt"

p \
  "<awk '($2==-3)' xvpl2el.out" u (($1-T0)*day/yr):(f($6)) w lp,\
  (0.0 + dotOmega2*(x*yr))/deg lw 2 lc 'orange',\
  (0.0 - dotOmega2*(x*yr))/deg lw 2 dt 2 lc 'gray'

pa -1

set term png small
set out "Omega2.png"
rep

q


