#!/usr/bin/gnuplot

set xl "u [cycles]"
set yl "v [cycles]"
set cbl "V^2 []"

tmp=7.e7
set xr [-tmp:tmp]
set yr [-tmp:tmp]
set cbr [-0.2:1.2]
set size ratio -1
set nokey
set zeroaxis
set colorbox
set palette rgbformulae 33,13,10

p "<awk '!/^ *#/{ i++; }(i==2){ print; }(NF==0){ i=0; }' chi2_VIS.dat" u (+$2/$4):(+$3/$4):6 pt 1 lc palette z,\
  "<awk '!/^ *#/{ i++; }(i==2){ print; }(NF==0){ i=0; }' chi2_VIS.dat" u (-$2/$4):(-$3/$4):6 pt 1 lc palette z

pa -1

set term png small
set out "uv.png"
rep


