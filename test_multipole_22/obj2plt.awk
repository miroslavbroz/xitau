#!/usr/bin/gawk -f

/^v/{
  i++;
  s[i]=$2 " " $3 " " $4;
}
/^f/{
  print s[$2] "\n" s[$3] "\n" s[$4] "\n" s[$2] "\n";
}

