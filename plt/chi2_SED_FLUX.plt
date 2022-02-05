#!/usr/bin/gnuplot

cm = 1.e-2  # m
mu = 1.e-6  # m
nm = 1.e-9  # m
k = 1.38e-23  # J K^-1
h = 6.63e-34  # J s
c = 299792458.  # m s^-1
R_S = 6.957e8  # m
M_S = 1.989e30  # kg
pc = 3.08567758e16  # m
G = 6.67430e-11  # kg^-2 m^3 s^-2

B_lambda(lambda,T) = 2.*h*c**2/lambda**5 * 1./(exp(h*c/(lambda*k*T))-1.)

load "T0.plt"

m1 = x_param1*M_S
m2 = x_param2*M_S
m3 = x_param3*M_S
m4 = x_param4*M_S
logg1 = x_param27
logg2 = x_param28
logg3 = x_param29
logg4 = x_param30
d_pc = x_param44*pc

T = 30000.  # K

R1 = sqrt(G*m1/(10.**logg1*cm))
R2 = sqrt(G*m2/(10.**logg2*cm))
R3 = sqrt(G*m3/(10.**logg3*cm))
R4 = sqrt(G*m4/(10.**logg4*cm))

print "R1 = ", R1/R_S, " R_S"
print "R2 = ", R2/R_S, " R_S"
print "R3 = ", R3/R_S, " R_S"
print "R4 = ", R4/R_S, " R_S"

########################################################################

set colors classic
set term x11

nm = 1.e-9

set xl "lambda_eff [nm]"
set yl "flux F [J s^-1 m^-2 m^-1]"

set logscale x
set logscale y

f(mag,calibration_flux) = 10.**(-0.4*mag)*calibration_flux

p \
  pi*B_lambda(x*nm,T)*(R1**2+R2**2+R3**2+R4**2)/d_pc**2 w l lt 0 lc 'black' t "Planck",\
  "<awk '!/^ *#/{ i++; }(i==1){ print; }(NF==0){ i=0; }' chi2_SED.dat" u ($1/nm):(f($3,$5)) w lp pt 1 lc 'orange' t "synthetic",\
  "Sed.dat" u ($1/nm):(f($3,$5)) w l lc 'blue' t "observed",\
  "Sed.dat" u ($1/nm):(f($3,$5)):($2/2/nm):(f($3+$4,$5)-f($3,$5)) not w xyerr ps 0 lc 'blue',\
  "chi2_SED.dat" u ($1/nm):(f($3,$5)) w l lw 3 lc 'red' t "residua",\
  "Sed.dat"      u ($1/nm):5 w l dt 2 lc 'gray' t "w. reddening"

pa -1

set term png small
set out "chi2_SED_FLUX.png"
rep

q


