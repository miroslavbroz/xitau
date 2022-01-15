#!/usr/bin/gnuplot

f(omega) = 2.*pi/omega

P0 = 0.224386764166667
omega0 = 2.*pi/P0

print "P0 = ", P0*24, " h = ", P0, " d  (multipole.in)"
print "P0 = 5.3852824(1) h  (Marchis etal.)"
print "omega0 = ", omega0, " rad d^-1"
print "omega0 = 28.0015861475352 rad d^-1  (spin.in)"

set xl "JD - 2400000"
set yl "P [h]"

set ytics format "%.8f"
set mouse format "%.8f"

load "T0.plt"
set arrow from T0-2400000,graph 0 rto 0,graph 1 nohead lt 0

p "<awk '($1==-1)' spin.out" u ($2-2400000):(f($6)*24) w lp,\
  (P0+1.89e-12*(x+2400000-T0))*24 w l lw 2 dt 2,\
  P0*24 w l lt 0

pa -1

q
