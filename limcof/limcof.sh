#!/bin/sh

./effwvl.awk effwvl.dat limcof_*.dat > limcof.tmp
sort -g -k4 -k3 -k2 -k1 < limcof.tmp > limcof.dat

./limcof.plt
#./limcof_lambda_Teff.plt

