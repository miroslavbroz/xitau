#!/usr/bin/env python3

day = 86400.  # s
clight = 299792458.  # m s^-1, exact
au = 1.49597870700e11  # m, from IAU 2012

f = open("V.dat", "w")
f.write("# t & mag & err & dataset\n")

T0 = 2458773.18865178
P = 1.0
d = 1.0
lite = -d/clight * au/day

n = 1000

for i in range(0,n+1):
  phase = float(i)/n
  t = T0 + phase*P - lite
  mag = 0.0
  err = 1.0
  dataset = 1
  f.write("%.6f  %.6f  %.6f  %d\n" % (t, mag, err, dataset))

f.close()

f = open("V.eph", "w")
f.write("# t & d & alpha & delta\n")

for i in range(0,n+1):
  phase = float(i)/n
  t = T0 + phase*P - lite
  alpha = 0.0*float(i)/n
  delta = 0.0*float(i)/n
  f.write("%.6f  %.6e  %12.6f  %12.6f\n" % (t, d, alpha, delta))

f.close()


