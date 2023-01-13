#!/usr/bin/gawk -f

(ARGIND==1) && (FNR>1) && !/^#/{
  i = $1;
  x[i] = $2;
  y[i] = $3;
  z[i] = $4;
}
(ARGIND!=1) && (FNR>1) && !/^#/{
  print x[$2], y[$2], z[$2];
  print x[$3], y[$3], z[$3];
  print x[$4], y[$4], z[$4];
  print x[$2], y[$2], z[$2];
  print "";
  print "";
}


