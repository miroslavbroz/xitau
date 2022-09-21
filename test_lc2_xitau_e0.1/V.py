#!/usr/bin/env python3

f = open("V.dat", "w")
f.write("# t & mag & err & dataset\n")

T0 = 2458773.18865178
P = 5.732436

n = 100

for i in range(0,n+1):
  phase = float(i)/n
  t = T0 + phase*P
  mag = 0.0
  err = 1.0
  dataset = 1
  f.write("%.6f  %.6f  %.6f  %d\n" % (t, mag, err, dataset))

f.close()


