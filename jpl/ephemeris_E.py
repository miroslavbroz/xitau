#!/usr/bin/env python3

import re
from astropy.time import Time

def hmsh(s):
  if s[0:1]=='-':
    sign = -1
  else:
    sign = +1
  l = s.split()
  h = abs(float(l[0]))
  m = float(l[1])
  s = float(l[2])
  return sign*(h + m/60.0 + s/3600.0)

def main():
  f = open("jpl.out", "r")
  f2 = open("ephemeris_E.dat", "w")
  f2.write("# topocentric ephemeris for astrometry\n")
  f2.write("# JD [TDB] & d [au] & alpha_j2000 [deg] & delta_j2000 [deg] & Besselian year [UTC] & dataset\n")

  j=0
  for line in f.readlines():

    # header
    if re.match("^ Date__", line):
      keys = line.replace(" ","").split(",")
      continue
    if re.match("^\$\$EOE", line):
      j=0
    if re.match("^\$\$SOE", line):
      j=1
      continue
    if j==0:
      continue

    # csv
    l = line.split(",")
    par = {}
    for i in range(0,len(l)):
      par[keys[i]] = l[i].strip()
    
    t1 = Time(par['Date_________JDUT'], scale='utc', format='jd')
    t2 = t1.tdb
    
    d = par['delta']
    alpha = hmsh(par['R.A._____(ICRF)'])/12.0*180.0
    delta = hmsh(par['DEC______(ICRF)'])
    bessel = 1900.0 + (t1.jd - 2415020.31352) / 365.242198781;
    dataset = "-";
    
    f2.write("%.8f	%s	%16.12f	%16.12f	%.6f	%s\n" % (t2.jd, d, alpha, delta, bessel, dataset))

  f.close()
  f2.close()

if __name__ == "__main__":
  main()

