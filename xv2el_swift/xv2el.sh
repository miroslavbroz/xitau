#!/bin/sh

./xv2el < out_JDATE_barycentric.dat > xv2el.out

./orbit.plt

#./diff.awk ../../e0.0__102/xv2el/xv2el.out xv2el.out > xv2el.diff

#./orbit_diff.plt

qiv orbit*.png


