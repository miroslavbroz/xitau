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
T1 = x_param16
T2 = x_param17
T3 = x_param18
logg1 = x_param19
logg2 = x_param20
logg3 = x_param21
d_pc = x_param33*pc

R1 = sqrt(G*m1/(10.**logg1*cm))
R2 = sqrt(G*m2/(10.**logg2*cm))
R3 = sqrt(G*m3/(10.**logg3*cm))

print "R1 = ", R1/R_S, " R_S"
print "R2 = ", R2/R_S, " R_S"
print "R3 = ", R3/R_S, " R_S"

########################################################################

set colors classic
set term x11

nm = 1.e-9  # m
ang = 1.e-10  # m
cm = 1.e-2  # m
erg = 1.e-7  # J

shift = 0.0

set xl "lambda [nm]"
set yl "F_lambda [erg s^-1 cm^-2 A^-1]"

set logscale y

p "1.abs" u ($1*ang/nm):($2+0*shift) t "1" w l,\
  "2.abs" u ($1*ang/nm):($2+1*shift) t "2" w l,\
  "3.abs" u ($1*ang/nm):($2+2*shift) t "3" w l,\
  "4.abs" u ($1*ang/nm):($2+3*shift) t "4" w l,\
  pi*B_lambda(x*nm,T1) / (erg*cm**(-2)*ang**(-1)) dt 2 lc 'gray' t "Planck",\
  pi*B_lambda(x*nm,T2) / (erg*cm**(-2)*ang**(-1)) dt 2 lc 'gray' not,\
  pi*B_lambda(x*nm,T3) / (erg*cm**(-2)*ang**(-1)) dt 2 lc 'gray' not

pa -1

set term png small size 1536,1024
set out "abs.png"
rep

q


