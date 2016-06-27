#!/usr/bin/gawk -f

BEGIN{
  GM_S = 0.2959122082855911e-03;  # AU^3/day^2, from JPLEPH DE405

  OFMT=CONVFMT="%22.16e";
  i=0;
  j=0;
}
(i==1) && (/^ *#/ || /^ [a-zA-Z]/){
  i=0;
}
(i==1){
  j=j+1;
  m[j] = $1;
  x[j] = $2;
  y[j] = $3;
  z[j] = $4;
  vx[j] = $5;
  vy[j] = $6;
  vz[j] = $7;
}
/barycentric/{
  i=1;
}
END{
  n=j;
  print n;
  for (j=1; j<=n; j++) {
    print GM_S*m[j];
    print x[j], y[j], z[j];
    print vx[j], vy[j], vz[j];
# heliocentric
#    print x[j]-x[1], y[j]-y[1], z[j]-z[1];
#    print vx[j]-vx[1], vy[j]-vy[1], vz[j]-vz[1];
  }
}

