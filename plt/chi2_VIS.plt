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
R1 = 1.0*R_S
R2 = 1.0*R_S
L1 = 1.0
L2 = 1.0
d_pc = 100.*pc

Irel = L2/L1
alpha1 = 5.3*R_S/d_pc
alpha2 = 0.0
theta1 = 2.*R1/d_pc
theta2 = 2.*R2/d_pc
theta_ = 1.0*mas

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

#set yr [:4]
set ytics 1
set mytics 1
set grid ytics mytics
set zeroaxis
set samples 10000

fac=2.0

# correction needed for Phoebe: sqrt(0.5)*...

p \
  "Vis.dat"        u (sqrt($2**2+$3**2)/$4):($6+fac*($8-1)):7 t "observed visibility" w err lt 3 pt 1 ps 0.5,\
  "visibility.dat" u (sqrt($2**2+$3**2)/$4):($5+fac*($6-1))   t "synthetic visibility" w p lt 7 pt 1,\
  "chi2_VIS.dat"   u (sqrt($2**2+$3**2)/$4):($6+fac*($8-1))   t "residua" w l lt 1 lw 1,\
  "<awk '($NF+0>100)' chi2_VIS.dat" u (sqrt($2**2+$3**2)/$4):($6+fac*($8-1)) t "chi^2 > 100" w p lt 1 pt 6 ps 1.5,\

pa -1

set term png small size 2048,1024
set out "chi2_VIS.png"
rep

q

  musq_binary(x,0,alpha1,0)-2.0 t sprintf("binary, alpha = %f mas", alpha1/mas) w l lc 'green',\
  musq_binary(x,0,alpha2,0)-2.0 t sprintf("binary, alpha = %f mas", alpha2/mas) w l lc 'gray',\
  musq_disk(x,0,theta1)-2.0      t sprintf("disk, theta1 = %f mas", theta1/mas) w l lc 'cyan',\
  musq_disk(x,0,theta_)-2.0      t sprintf("disk, theta_ = %f mas", theta_/mas) w l lc 'gray'
  "<awk '($1<0.0+0.5)' test_xitau_agreement.out" u (sqrt($2**2+$3**2)/$4):($5+fac*0) t "Phoebe test" w l lc 'black' lw 3 dt 2,\
  "<awk '($1>0.0+0.5)' test_xitau_agreement.out" u (sqrt($2**2+$3**2)/$4):($5+fac*1) not             w l lc 'black' lw 3 dt 2,\

