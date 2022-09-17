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
m4 = x_param4*M_S
P1 = x_param5*day
e1 = x_param6
i1 = x_param7*deg
Omega1 = x_param8*deg
P2 = x_param11*day
e2 = x_param12
i2 = x_param13*deg
Omega2 = x_param14*deg

a1 = (G*(m1+m2)/(4.*pi**2)*P1**2)**(1./3.)
a2 = (G*(m1+m2+m3)/(4.*pi**2)*P2**2)**(1./3.)
m1_ = m1*m2/(m1+m2)
m2_ = (m1+m2)*m3/(m1+m2+m3)
n1 = 2.*pi/P1
n2 = 2.*pi/P2
L1 = m1_*n1*a1**2
L2 = m2_*n2*a2**2
cosJ = cos(i1)*cos(i2) + sin(i1)*sin(i2)*cos(Omega1-Omega2)
eta2 = sqrt(1.-e2**2)
gamma = L1/(L2*eta2)

dotOmega2 = n2*3./(4.*eta2**3)*m3/(m1+m2+m3)*n2/n1*cosJ * sqrt(1. + gamma**2 + 2.*gamma*cosJ)  # Nemravova etal. (2016), Eq. (27)

print "a1 = ", a1/au, " au"
print "a2 = ", a2/au, " au"
print "n1 = ", n1, " rad s^-1"
print "n2 = ", n2, " rad s^-1"
print "eta2 = ", eta2
print "cosJ = ", cosJ
print "J = ", acos(cosJ)/deg, " deg"
print "gamma = ", gamma
print "dotOmega2 = ", dotOmega2, " rad s^-1 = ", dotOmega2/(deg/yr), " deg y^-1"

f(x) = x > 180. ? x - 360. : x

set xl "time [yr]"
set yl "Omega_2 [deg]"

set zeroaxis

load "T0.plt"

p \
  "<awk '($2==-3)' xvpl2el.out" u (($1-T0)*day/yr):(f($6)) w lp,\
  (-32.0*deg + dotOmega2*(x*yr))/deg lw 2 lc 'orange',\
  (-32.0*deg - dotOmega2*(x*yr))/deg lw 2 dt 2 lc 'gray'
  

pa -1

set term png small
set out "omega2.png"
rep

q


