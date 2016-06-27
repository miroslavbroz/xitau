#!/usr/bin/gnuplot

deg = pi/180.
arcsec = deg/3600.
mas = arcsec/1.e3

AU = 1.49597870700e11  # m, from IAU 2012
pc = 648000.e0/pi*AU  # m, from IAU 2015
R_S = 6.957e8  # m, from IAU 2015

d = 1.*AU

print "d = ", d/pc, " pc"

# Apparent V magnitude		 V		 -26.71 mag	 0.02
# B-V color (Johnson)	 	 (B-V)Sun	 0.653 mag	 0.003
# U-B color (Johnson)	 	 (U-B)Sun	 0.158 mag	 0.009
# V-Rc color (Johnson,Cousins)	 (V-Rc)Sun	 0.356 mag	 0.003
# V-Ic color (Johnson,Cousins)	 (V-Ic)Sun	 0.701 mag	 0.003
# V-J color (Johnson,2MASS)	 (V-J)Sun	 1.198 mag	 0.005
# V-H color (Johnson,2MASS)	 (V-H)Sun	 1.484 mag	 0.009
# V-Ks color (Johnson,2MASS)	 (V-Ks)Sun	 1.560 mag	 0.008
# J-H color (2MASS)	 	 (J-H)Sun	 0.286 mag	 0.01
# J-Ks color (2MASS)	 	 (J-Ks)Sun	 0.362 mag	 0.01

V = -26.71  # mag, from https://sites.google.com/site/mamajeksstarnotes/basic-astronomical-data-for-the-sun

B_V = 0.648  # mag, from Gray (1995)
R_I = 0.338  # mag, from Gray (1995)

B_V  = 0.653
U_B  = 0.158
V_Rc = 0.356
V_Ic = 0.701
V_J  = 1.198
V_H  = 1.484
V_Ks = 1.560

B  = B_V + V
U  = U_B + B
Rc = V - V_Rc
Ic = V - V_Ic
J  = V - V_J
H  = V - V_H
Ks = V - V_Ks

sigma_V = 0.02
sigma_B_V  = 0.003
sigma_U_B  = 0.009
sigma_V_Rc = 0.003
sigma_V_Ic = 0.003
sigma_V_J  = 0.005
sigma_V_H  = 0.009
sigma_V_Ks = 0.008

sigma_B  = sqrt(sigma_B_V**2 + sigma_V**2)
sigma_U  = sqrt(sigma_U_B**2 + sigma_B**2)
sigma_Rc = sqrt(sigma_V**2 + sigma_V_Rc**2)
sigma_Ic = sqrt(sigma_V**2 + sigma_V_Ic**2)
sigma_J  = sqrt(sigma_V**2 + sigma_V_J**2)
sigma_H  = sqrt(sigma_V**2 + sigma_V_H**2)
sigma_Ks = sqrt(sigma_V**2 + sigma_V_Ks**2)

print "U = ", U, " mag"
print "B = ", B, " mag"
print "V = ", V, " mag"
print "Rc = ", Rc, " mag"
print "Ic = ", Ic, " mag"
print "J = ", J, " mag"
print "H = ", H, " mag"
print "Ks = ", Ks, " mag"

print "sigma_U = ", sigma_U, " mag"
print "sigma_B = ", sigma_B, " mag"
print "sigma_V = ", sigma_V, " mag"
print "sigma_Rc = ", sigma_Rc, " mag"
print "sigma_Ic = ", sigma_Ic, " mag"
print "sigma_J = ", sigma_J, " mag"
print "sigma_H = ", sigma_H, " mag"
print "sigma_Ks = ", sigma_Ks, " mag"


