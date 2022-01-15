#!/usr/bin/gawk -f

BEGIN{
  OFMT="%.16e";
}
(ARGIND==1) && !/^#/{
  s[$1,$2] = $0;
}

(ARGIND>1){
  split(s[$1,$2], l);
  print $1, $2, $3-l[3], $4-l[4], $5-l[5], f($6-l[6]), f($7-l[7]), f($8-l[8]);
}

func f(x) {
  if (x<-180.) {
    return x+360.;
  } else if (x>180.) {
    return x-360.;
  } else {
    return x;
  }
}

