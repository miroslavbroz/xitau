#!/usr/bin/gnuplot

# kopal2.plt, equivalent to kopal1.plt, with inverted q and coordinates

q = 0.333
q = 1.0/q

r1(x,y,z) = sqrt(x**2+y**2+z**2)
r2(x,y,z) = sqrt((x-1.)**2+y**2+z**2)

Omega(x,y,z) = 1./r1(x,y,z) + q/r2(x,y,z) + 1./2.*(1.+q)*(x**2+y**2) - q*x + q**2/(2.*(1.+q))

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

sp 1./q*Omega(1.-x,y,0.0)

pa -1


