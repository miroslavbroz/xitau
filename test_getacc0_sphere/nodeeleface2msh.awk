#!/usr/bin/gawk -f

(ARGIND==1) && (FNR>1) && !/^#/{
  nv++;
  vx[nv]=$2;
  vy[nv]=$3;
  vz[nv]=$4;
}
(ARGIND==2) && (FNR>1) && !/^#/{
  ne++;
  e1[ne]=$2;
  e2[ne]=$3;
  e3[ne]=$4;
  e4[ne]=$5;
}
(ARGIND==3) && (FNR>1) && !/^#/{
  nf++;
  f1[nf]=$2;
  f2[nf]=$3;
  f3[nf]=$4;
}
END{
  print nv, ne, nf;

  for (i=1; i<=nv; i++) {
    print vx[i], vy[i], vz[i], 0;
  }
  for (i=1; i<=ne; i++) {
    print e1[i], e2[i], e3[i], e4[i], 0;
  }
  for (i=1; i<=nf; i++) {
    print f1[i], f2[i], f3[i], 0;
  }
}

