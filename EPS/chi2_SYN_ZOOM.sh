#!/bin/sh

FILE=`echo $0 | awk 'BEGIN{FS=".";}{s=$1; for(i=2;i<=NF-1;i++){s=s "." $i;} print s;}'`

./$FILE.plt
patch $FILE.eps $FILE.eps.diff

