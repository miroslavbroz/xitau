#!/usr/bin/gnuplot

deg = pi/180.  # rad
arcsec = deg/3600.
mas = 1.e-3*arcsec
R_S = 6.957e8  # m
pc = 3.08567758e16  # m

load "T0.plt"

#m1 = x_param1*M_S
#m2 = x_param2*M_S
#m3 = x_param3*M_S
R1 = 13.936706168245422*R_S
R2 = 2.6368872269752930*R_S
R3 = 9.9569008963121579*R_S
L1 = 0.63923176264951931     
L2 = 2.1033415508141581E-002
L3 = 0.33973482184233911     
d_pc = 382.*pc

Irel = L3/L1
alpha1 = 0.32*arcsec
alpha2 = 0.00052*arcsec
theta1 = 2.*R1/d_pc
theta2 = 2.*R2/d_pc
theta_ = 1.9*mas

print "alpha1 = ", alpha1, " rad = ", alpha1/arcsec, " arcsec"
print "alpha2 = ", alpha2, " rad = ", alpha2/arcsec, " arcsec"
print "theta1 = ", theta1, " rad = ", theta1/arcsec, " arcsec"
print "theta2 = ", theta2, " rad = ", theta2/arcsec, " arcsec"
print "theta' = ", theta_, " rad = ", theta_/arcsec, " arcsec"

musq_binary(u,v,u0,v0) = (1.+Irel**2+2.*Irel*cos(2.*pi*(u*u0+v*v0)))/(1.+Irel)**2

# missing factor 2!
musq_disk(u,v,theta) = (2.*besj1(pi*theta*sqrt(u**2+v**2))/(pi*theta*sqrt(u**2+v**2)))**2

########################################################################

set colors classic
set term x11

set xl "B/lambda [cycles]"
set yl "V^2 [] (shifted by dataset number)"

set ytics 1
set mytics 1
set grid ytics mytics
set zeroaxis
set samples 10000

fac=2.0

p \
  "Vis.dat"        u (sqrt($2**2+$3**2)/$4):($6+fac*($8-1)):7 t "observed visibility" w err lt 3 pt 1 ps 0.5,\
  "visibility.dat" u (sqrt($2**2+$3**2)/$4):($5+fac*($6-1))   t "synthetic visibility" w p lt 7 pt 1,\
  "chi2_VIS.dat"   u (sqrt($2**2+$3**2)/$4):($6+fac*($8-1))   t "residua" w l lt 1 lw 1,\
  musq_binary(x,0,alpha1,0)-2.0 t sprintf("binary, alpha = %f mas", alpha1/mas) w l lc 'gray',\
  musq_binary(x,0,alpha2,0)-2.0 t sprintf("binary, alpha = %f mas", alpha2/mas) w l lc 'green',\
  musq_disk(x,0,theta1)-2.0      t sprintf("disk, theta1 = %f mas", theta1/mas) w l lc 'cyan',\
  musq_disk(x,0,theta_)-2.0      t sprintf("disk, theta_ = %f mas", theta_/mas) w l lc 'cyan',\

pa -1

set term png small size 2048,1024
set out "chi2_VIS.png"
rep

q

  "<awk '($NF+0>100)' chi2_VIS.dat" u (sqrt($2**2+$3**2)/$4):($6+$8) t "chi^2 > 100" w p lt 1 pt 6 ps 1.5


