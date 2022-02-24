#!/usr/bin/gnuplot

# equivalent to kopal3.plt, with modified potential for the secondary

# Reference: Harmanec (2021), Dvojhvezdy, ast007.pdf, Eq. (13)

q = 0.333

r1(x,y,z) = sqrt(x**2+y**2+z**2)
r2(x,y,z) = sqrt((x-1.)**2+y**2+z**2)

Omega(x,y,z) = 1./r1(x,y,z) + q/r2(x,y,z) + 1./2.*(1.+q)*(x**2+y**2) - q*x
Omega_(x,y,z) = 1./r1(x,y,z) + 1./(q*r2(x,y,z)) + 1./2.*(1.+q)/q*(x**2+y**2) - x/q
Omega__(x,y,z) = Omega(x,y,z)/q + (q-1.)/(2.*q)

term1 = 1.
term2 = q
term3 = 1./2.*(1.+q)
term4 = q
term5 = q**2/(2.*(1+q))

print "term1 = ", term1
print "term2 = ", term2
print "term3 = ", term3
print "term4 = ", term4
print "term5 = ", term5

########################################################################

tmp=1.0
set xr [-tmp:1.+tmp]
set yr [-0.5-tmp:0.5+tmp]
set zr [0:30]

set zeroaxis
set samples 100
set isosamples 100,100
set xyplane 0.0
set view equal xy
set nokey

set cntrparam levels incremental 0,0.1,20
set nosurface
set contours surface
set view 0,0
#set view map

sp q*(Omega_(x,y,0.0)+1./(2.*q*(1.+q)))

pa -1


