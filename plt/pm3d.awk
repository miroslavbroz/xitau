#!/usr/bin/gawk -f

(ARGIND==1) && (FNR>1) && !/^#/{
  i = $1;
  x[i] = $2;
  y[i] = $3;
  z[i] = $4;
}
(ARGIND==2) && (FNR>1) && !/^#/{
  i = $1;
  f1[i] = $2; 
  f2[i] = $3; 
  f3[i] = $4; 
}
(ARGIND==3) && (FNR>1) && !/^#/{
  j = $1;
  print x[f1[j]], y[f1[j]], z[f1[j]], $0;
  print x[f2[j]], y[f2[j]], z[f2[j]], $0;
  print "";
  print x[f3[j]], y[f3[j]], z[f3[j]], $0;
  print x[f3[j]], y[f3[j]], z[f3[j]], $0;
  print "";
  print "";
}

