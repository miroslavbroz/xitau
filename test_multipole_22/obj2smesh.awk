#!/usr/bin/awk -f

# obj2smesh.awk
# Convert OBJ file to a SMESH format (i.e. a tetgen input).
# Miroslav Broz (miroslav.broz@email.cz), Nov 25th 2012

/^v/{
  n++;
  vx[n]=$2;
  vy[n]=$3;
  vz[n]=$4;
}
/^f/{
  m++;
  f1[m]=$2;
  f2[m]=$3;
  f3[m]=$4;
}
END{
  print "# part 1, node list";
  print n " 3 0 0";
  for (i=1; i<=n; i++) {
    print i "	" vx[i] "	" vy[i] "	" vz[i];
  }
  print "";

  print "# part 2, facet list";
  print m " 0";
  for (j=1; j<=m; j++) {
    print "3	" f1[j] "	" f2[j] "	" f3[j];
  }
  print "";

  print "# part 3, hole list\n0\n";
  print "# part 4, region list\n0\n";
}

