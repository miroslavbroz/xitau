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

msum = x_param1*M_S
q1 = x_param2
P1 = x_param3*day
e1 = 10.**x_param4
i1 = x_param5*deg
g1 = 10.**x_param11*cm
C20 = x_param19

m1 = msum/(1.+q1)
m2 = msum-m1
i1 = 0.*deg  # wrt. equator

J2 = -C20  # see also multipole.in; J2 = -Clm(2,0)

R1 = sqrt(G*m1/g1)
a1 = (G*(m1+m2)/(4.*pi**2)*P1**2)**(1./3.)
n1 = sqrt(G*(m1+m2)/a1**3)
eta1 = sqrt(1.-e1**2)

dotomega = +3.*n1*J2*(R1/a1)**2 * (5.*cos(i1)**2-1.)/(4.*eta1**4)  # standard coefficients; Bertotti etal. (2003), p. 332

# Note: Fabrycky (2010) does NOT contain non-radial components!!

#dotomega = dotomega/6.  # additional factor (cf. below)

print "m1 = ", m1/M_S, " M_S"
print "m2 = ", m2/M_S, " M_S"
print "R1 = ", R1/R_S, " R_S"
print "a1 = ", a1/au, " au = ", a1/R_S, " R_S"
print "n1 = ", n1, " rad s^-1"
print "J2 = ", J2, "  <-- cf. 2.e-7"
print "eta1^4 = ", eta1**4
print "(5 cos^2 i-1)/4 = ", (5.*cos(i1)**2-1.)/4.
print "dotomega = ", dotomega, " rad s^-1 = ", dotomega/(deg/yr), " deg y^-1"
print "1/6 dotomega = ", 1./6.*dotomega, " rad s^-1 = ", 1./6.*dotomega/(deg/yr), " deg y^-1"
print ""

f(x) = x > 180. ? x - 360. : x

set xl "time [yr]"
set yl "omega_1 [deg]"

set zeroaxis

load "T0.plt"

p \
  (0.0 + 1.45*deg/yr*(x*yr))/deg lw 2 lc 'green',\
  (0.0 + dotomega*(x*yr))/deg lw 2 lc 'orange',\
  (0.0 + 1./6.*dotomega*(x*yr))/deg lw 2 dt 2 lc 'gray'

pa -1

set term png small
set out "omega1.png"
rep

q

  "xvpl2el.out" u (($1-T0)*day/yr):(f($7)) w lp,\

