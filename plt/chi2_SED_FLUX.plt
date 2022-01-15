#!/usr/bin/gnuplot

set term x11

nm = 1.e-9

set xl "lambda_eff [nm]"
set yl "flux F [J s^-1 m^-2 m^-1]"

#set xr [300:800]
set yr [0:]

f(mag,calibration_flux) = 10.**(-0.4*mag)*calibration_flux

p \
  "chi2_SED.dat" u ($1/nm):(f($3,$5)) t "synthetic" w p pt 1 lt 8,\
  "Sed.dat" u ($1/nm):(f($3,$5)) t "observed" w l   lt 3,\
  "Sed.dat" u ($1/nm):(f($3,$5)):($2/2/nm):(f($3+$4,$5)-f($3,$5)) not w xyerr lt 3 ps 0,\
  "chi2_SED.dat" u ($1/nm):(f($3,$5)) t "residua" w l lt 1 lw 3
pa -1

set term png small
set out "chi2_SED_FLUX.png"
rep

q


