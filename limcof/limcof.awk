#!/usr/bin/gawk -f

# limcof.awk
# Grep u_limb(lambda) linear limb-darkening coefficient vs wavelength for a given T_eff, log g and Z.
# Miroslav Broz (miroslav.broz@email.cz), Jun 8th 2016

BEGIN{
  Teff = ARGV[1];
  logg = ARGV[2];
  Z = ARGV[3];
  ARGV[1]=ARGV[2]=ARGV[3] = "";
  eps = 1.e-6;
}
(abs($2-Teff) < eps) && (abs($3-logg) < eps) && (abs($4-Z) < eps){
  print;
}

func abs(x) {
  if (x > 0) {
    return x;
  } else {
    return -x;
  }
}

