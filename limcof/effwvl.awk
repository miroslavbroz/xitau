#!/usr/bin/gawk -f

# effwvl.awk
# Supply effective wavelengths (of broad-band filters) to limb-darkening data.
# Miroslav Broz (miroslav.broz@email.cz), Jun 8th 2016

# Note: effwvl.dat is from a newer version of WD 2015 and we use only first 16 bands.

BEGIN{
  print "# lambda_eff [m] & T_eff [K] & log(g) [cgs] & metallicity Z [M/H] & xlin linear limb-darkening coefficient"
}
(ARGIND==1){
  eff_lambda[$1] = $2*1.e-9;
}
(ARGIND>1) && /Teff/{
  Teff = $3;
  logg = $8;
  Z = $11;
}
(ARGIND>1) && !/Teff/ && (NF>0){
  filter = $1;
  xlin = $2;
  if (xlin > 1.0) {
    xlin = 1.0;
  }
  i++;
  if (filter == "bolo") { i = 0; }

#  print filter, eff_lambda[i], Teff, logg, Z, xlin;

  if ((i>=1) && (i<=16)) {
    printf("%.6e  %5.0f  %3.1f  %4.1f  %5.3f  %s\n", eff_lambda[i], Teff, logg, Z, xlin, filter);
  }
}

