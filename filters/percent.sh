#!/bin/sh

exit

for FILE in \
  geneva_1962_*.txt \
  stromgren_1956_*.txt \
  ; do

  F="${FILE%.txt}"
  echo $F

  ./percent.awk < $FILE > $F.dat 
  
done

