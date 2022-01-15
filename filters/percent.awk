#!/usr/bin/gawk -f

/^#/{
  print;
}
!/^#/{
  printf("%4.0f %6.4f\n", $1, $2/100.);
}

