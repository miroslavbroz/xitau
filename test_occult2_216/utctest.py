#!/usr/bin/env python3

import sys
from astropy.time import Time

def jdate(time):
    l = time.split()
    y, m, d = l[0].split("-")
    h, m_, s = l[1].split(":")
    y = float(y)
    m = float(m)
    d = float(d)
    h = float(h)
    m_ = float(m_)
    s = float(s)
    d = d+(h+m_/60.+s/3600.)/24.

    if (m > 2):
        yy = y
        mm = m
    else:
        yy = y - 1.0
        mm = m + 12.0
    jd = int(365.25*yy) + int(30.6001*(mm+1)) + d + 1720994.5

    if (y > 1582) or ((y == 1582) and (m > 10)) or ((y == 1582) and (m == 10) and (int(d) > 15)):
        a = int(yy/100.0)
        b = 2.0-a+int(a/4.0)
        jd = jd + b

    return jd

def main():

#    utc = []
#    month = [31,28,31,30,31,30,31,31,30,31,30,31]
#    for m in xrange(8,9+1):
#        for d in xrange(1,month[m]):
#            for h in xrange(0,24):
#                utc.append("2020-%02d-%02d %02d:00:00" % (m, d, h))
#
#    f = open("utctest.in", "w")
#    for tmp in utc:
#        f.write(tmp + "\n")
#    f.close()

    utc = []
    f = open("utctest.in", "r")
    for line in f.readlines():
        l = line.split()
        utc.append(l[0] + ' ' + l[1])
    f.close()

#    print "# JD_old & JD_UTC"
#    for t in utc:
#        if len(t) < 2:
#            continue
#
#        t1 = Time(t, format="iso", scale="utc")
#        print "%.6f == %.6f" % (jdate(t), t1.jd)

    f = open("utctest.out", "w")
    f.write("# JD_UTC & JD_TDB & TDB-UTC [s]\n")
    for t in utc:
        if len(t) < 2:
            continue

        t1 = Time(t, format="iso", scale="utc")
        t2 = t1.tdb

        f.write("%.6f -> %.6f  %.6f s\n" % (t1.jd, t2.jd, (t2.jd-t1.jd)*86400.))
    f.close()

if __name__ == "__main__":
    main()



