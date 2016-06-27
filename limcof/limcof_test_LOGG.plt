#!/usr/bin/gnuplot

set term x11

set xl "lambda [nm]"
set yl "log g [cgs]"
set zl "u_limb []"

set ticslevel 0
set view 90,0

load "limcof_test.lab"

sp "limcof_test.out" u 1:(logg):2 w lp lw 2,\
   sprintf("<./limcof.awk %f %f %f limcof.dat", Teff1, logg1, Z1) u 1:3:5 not w l,\
   sprintf("<./limcof.awk %f %f %f limcof.dat", Teff2, logg1, Z1) u 1:3:5 not w l,\
   sprintf("<./limcof.awk %f %f %f limcof.dat", Teff1, logg2, Z1) u 1:3:5 not w l,\
   sprintf("<./limcof.awk %f %f %f limcof.dat", Teff2, logg2, Z1) u 1:3:5 not w l,\
   sprintf("<./limcof.awk %f %f %f limcof.dat", Teff1, logg1, Z2) u 1:3:5 not w l,\
   sprintf("<./limcof.awk %f %f %f limcof.dat", Teff2, logg1, Z2) u 1:3:5 not w l,\
   sprintf("<./limcof.awk %f %f %f limcof.dat", Teff1, logg2, Z2) u 1:3:5 not w l,\
   sprintf("<./limcof.awk %f %f %f limcof.dat", Teff2, logg2, Z2) u 1:3:5 not w l

pa -1


