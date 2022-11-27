#!/usr/bin/env python3

import sys

def effective(w, f):
  n = len(w)
  s1 = 0.0
  for i in range(0,n-1):
    s1 += 0.5*(f[i]+f[i+1])*(w[i+1]-w[i])

  Delta_eff = s1

  s2 = 0.0
  for i in range(0,n-1):
    s2 += 0.5*(f[i]+f[i+1])*0.5*(w[i]+w[i+1])*(w[i+1]-w[i])

  lambda_eff = s2/s1

  return lambda_eff, Delta_eff

def main():
  w = []
  f = []
  for line in sys.stdin:
    l = line.split()
    if l[1] == "99.99":
      continue
    w.append(float(l[0])*1.0e-9)
    f.append(float(l[1]))

  lambda_eff, Delta_eff = effective(w, f)

  print("lambda_eff = ", lambda_eff, " m")
  print("Delta_eff = ", Delta_eff, " m")

if __name__ == "__main__":
  main()


