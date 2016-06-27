#!/usr/bin/gnuplot

AU = 1.49597870700e11  # m, from IAU 2012
pc = 648000.e0/pi*AU   # m, from IAU 2015
c = 2.99792458e8       # m/s
h = 6.626070040e-34    # J s, from 2014 CODATA
k_B = 1.38064852e-23   # J K^-1, from 2014 CODATA
R_S = 6.957e8          # m, from IAU 2015
nm = 1.e-9  # m
A = 1.e-10  # m

T_Vega = 9550.           # K, efektivni teplota Slunce
R_Vega = 2.78*R_S
d = 7.75614674629543*pc

B(lambda,T) = 2.*h*c**2/lambda**5 * 1 / (exp(h*c/(k_B*T*lambda)) - 1)

########################################################################

set term x11

nm = 1.e-9

set xl "lambda_eff [nm]"
set yl "UBV magnitude [mag]"
set y2l "irradiance / W m^-2 m^-1"

set xr [100:2200]
set yr [:0.6] reverse
set y2r [:0.30]
set ytics 0.1
set ytics nomirror
set y2tics
set zeroaxis
set key left

p \
  "<awk '!/^ *#/ && (NF>0){ i++; } (i%2==1){ print; }' chi2_SED.dat" u ($1/nm):3 t "real solar spectrum" w lp pt 1 lt 1,\
  "<awk '!/^ *#/ && (NF>0){ i++; } (i%2==1){ print; }' ../test_vega0_planck/chi2_SED.dat" u ($1/nm):3 t "Planck approximation" w lp pt 1 lt 7,\
  "<awk '!/^ *#/ && (NF>0){ i++; } (i%2==1){ print; }' ../test_vega1_spectrum/chi2_SED.dat" u ($1/nm):3 t "effective wavelength/bandpass" w lp pt 1 lt 2,\
  "Sed.dat" u ($1/nm):3   t "observed" w l   lt 3,\
  "Sed.dat" u ($1/nm):3:4 not          w err lt 3 ps 0,\
  "alpha_lyr_stis_002.dat" u ($1*1.e-10/nm):2 ax x1y2 w l lt 1,\
  4.*pi*R_Vega**2 * pi*B(x*nm,T_Vega) / (4.*pi*d**2) ax x1y2 not w l lt 7

pa -1

set term png small
set out "chi2_SED.png"
rep

q

  "chi2_SED.dat" u ($1/nm):3 t "residua" w l lt 1 lw 3

