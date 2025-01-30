#!/bin/sh

./xvpl2el < out_JDATE_barycentric.dat > xvpl2el.out

./orbit.plt

./diff.awk ../../e0.0__102/xvpl2el/xvpl2el.out xvpl2el.out > xvpl2el.diff

./orbit_diff.plt

qiv orbit*.png


