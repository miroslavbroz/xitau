#!/usr/bin/gawk -f

BEGIN{
  i = 0;
}
($3==1){
  s = $0;
  i = 1;
}
(NF!=0){
  print;
}
(NF==0) && (i==1){
  print s;
  i = 0;
}
(NF==0){
  print $0;
}

