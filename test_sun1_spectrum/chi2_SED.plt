#!/usr/bin/gnuplot

AU = 1.49597870700e11  # m, from IAU 2012
c = 2.99792458e8       # m/s
h = 6.626070040e-34    # J s, from 2014 CODATA
k_B = 1.38064852e-23   # J K^-1, from 2014 CODATA
R_S = 6.957e8          # m, from IAU 2015
nm = 1.e-9

T_Sun = 5780.           # K, efektivni teplota Slunce

B(lambda,T) = 2.*h*c**2/lambda**5 * 1 / (exp(h*c/(k_B*T*lambda)) - 1)

########################################################################

set term x11

nm = 1.e-9

set xl "lambda_eff [nm]"
set yl "UBV magnitude [mag]"
set y2l "irradiance / W m^-2 nm^-1"

set xr [200:2200]
set yr [:] reverse
set ytics 0.1
set ytics nomirror
set y2tics
set key left

p \
  "<awk '!/^ *#/ && (NF>0){ i++; } (i%2==1){ print; }' chi2_SED.dat" u ($1/nm):3 t "real solar spectrum" w lp pt 1 lt 1,\
  "<awk '!/^ *#/ && (NF>0){ i++; } (i%2==1){ print; }' ../test_sun0_planck/chi2_SED.dat" u ($1/nm):3 t "Planck approximation" w lp pt 1 lt 7,\
  "Sed.dat" u ($1/nm):3   t "observed" w l   lt 3,\
  "Sed.dat" u ($1/nm):3:4 not          w err lt 3 ps 0,\
  "NewGuey2003.txt" u 1:2 ax x1y2 w l lt 1,\
  4.*pi*R_S**2 * pi*B(x*nm,T_Sun) / (4.*pi*AU**2) * nm ax x1y2 not w l lt 7


pa -1

set term png small
set out "chi2_SED.png"
rep

q


  "chi2_SED.dat" u ($1/nm):3 t "residua" w l lt 1 lw 3

