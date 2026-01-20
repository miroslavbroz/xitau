#!/usr/bin/env gawk -f

BEGIN{
  n = 64;
  pi = 3.1415926535; deg = pi/180.; rad = 180./pi;
}
!/^ *#/ && (NF>0){
  x = $2;
  y = $3;
  a = $4;
  b = $5;
  PA_ellipse = $6;
  d = $7;

#  phi2 = PA_ellipse + pi/2.;
  phi2 = -PA_ellipse - pi/2.;

  for (i=0; i<=n; i++) {
    phi = 2.*pi*i/n;
    x2 = a*cos(phi);
    y2 = b*sin(phi);
    x3 =  x2*cos(phi2) + y2*sin(phi2)
    y3 = -x2*sin(phi2) + y2*cos(phi2)
    x4 = x + x3;
    y4 = y + y3;
    print x4, y4, d;
  }
  print "";

}

