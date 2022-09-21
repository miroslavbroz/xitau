#!/usr/bin/env python3

f = open("V.dat", "w")
f.write("# phase & mag & err & dataset\n")

n = 1000
for i in range(0,n+1):
  phase = float(i)/n
  mag = 0.0
  err = 1.0
  dataset = 1
  f.write("%.6f  %.6f  %.6f  %d\n" % (phase, mag, err, dataset))

f.close()


