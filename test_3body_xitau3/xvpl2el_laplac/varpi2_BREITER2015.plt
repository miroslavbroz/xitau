#!/usr/bin/gnuplot

# References:
# Breiter & Vokrouhlicky (2015)
# Nemravova etal. (2016)

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
e1 = x_param5
i1 = x_param6*deg
Omega1 = x_param7*deg
P2 = x_param10*day
e2 = x_param11
i2 = x_param12*deg
Omega2 = x_param13*deg

a1 = (G*(m1+m2)/(4.*pi**2)*P1**2)**(1./3.)
a2 = (G*(m1+m2+m3)/(4.*pi**2)*P2**2)**(1./3.)
m1_ = m1*m2/(m1+m2)
m2_ = (m1+m2)*m3/(m1+m2+m3)
n1 = 2.*pi/P1
n2 = 2.*pi/P2
L1 = m1_*n1*a1**2
L2 = m2_*n2*a2**2
cosJ = cos(i1)*cos(i2) + sin(i1)*sin(i2)*cos(Omega1-Omega2)
J = acos(cosJ)
eta2 = sqrt(1.-e2**2)
gamma = L1/(L2*eta2)

dotvarpi2 = 3./8.*n2/(eta2**3)*m3/(m1+m2+m3)*n2/n1*gamma * (3.*cosJ**2 - 1. - gamma*sin(J)*sin(2.*J)/(1. + gamma*cosJ + sqrt(1.+gamma**2+2.*gamma*cosJ)))  # Nemravova etal. (2016), Eq. (28)

print "a1 = ", a1/au, " au"
print "a2 = ", a2/au, " au"
print "n1 = ", n1, " rad s^-1"
print "n2 = ", n2, " rad s^-1"
print "eta2 = ", eta2
print "cosJ = ", cosJ
print "J = ", J/deg, " deg"
print "gamma = ", gamma
print "dotvarpi2 = ", dotvarpi2, " rad s^-1 = ", dotvarpi2/(deg/yr), " deg y^-1"

f(x) = x > 180. ? x - 360. : x < -180 ? x + 360. : x

set xl "time [yr]"
set yl "Omega_2 [deg]"

set zeroaxis
set key left

load "T0.plt"

p \
  "<awk '($2==-3)' xvpl2el.out" u (($1-T0)*day/yr):(f(f($6+$7))) w lp,\
  (-32.0*deg + dotvarpi2*(x*yr))/deg lw 2 lc 'orange'
  

pa -1

set term png small
set out "varpi2.png"
rep

q

  "<awk '($2==-3)' xvpl2el.out" u (($1-T0)*day/yr):(f(-f($6)+f($7))) w lp,\

