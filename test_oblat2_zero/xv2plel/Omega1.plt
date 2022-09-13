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
P1 = x_param3*day
e1 = x_param4
i1 = x_param5*deg
g1 = 10.**x_param11*cm

J2 = 0.0  # see multipole.in; J2 = -Clm(2,0)

R1 = sqrt(G*m1/g1)
a1 = (G*(m1+m2)/(4.*pi**2)*P1**2)**(1./3.)
n1 = sqrt(G*(m1+m2)/a1**3)
eta1 = sqrt(1.-e1**2)
dotOmega = -3./2.*n1*J2*(R1/a1)**2*cos(i1)/eta1**4  # standard coefficients

print "i1 = ", i1/deg, " deg"
print "R1 = ", R1/R_S, " R_S"
print "a1 = ", a1/au, " au"
print "n1 = ", n1, " rad s^-1"
print "J2 = ", J2, "  <-- cf. 2.e-7"
print "dotOmega = ", dotOmega, " rad s^-1"

f(x) = x > 180. ? x - 360. : x

set xl "time [yr]"
set yl "Omega_1 [deg]"

set zeroaxis
set key left

load "T0.plt"

p \
  "xvpl2el.out" u (($1-T0)*day/yr):(f($6)) w lp,\
  "../../test_oblat6_63.4deg/xvpl2el/xvpl2el.out" u (($1-T0)*day/yr):(f($6)) w lp,\
  "../../test_oblat7_80deg/xvpl2el/xvpl2el.out" u (($1-T0)*day/yr):(f($6)) w lp,\
  (0.0 + dotOmega*(x*yr))/deg lw 2 lc 'orange',\
  (0.0 - dotOmega*(x*yr))/deg lw 2 dt 2 lc 'gray'

pa -1

set term png small
set out "Omega1.png"
rep

q


