#!/usr/bin/gnuplot

set xl "x [au]"
set yl "y [au]"

set zeroaxis
set size ratio -1
set palette rgbformulae 33,13,10

p \
  "<awk '($2==-2) && (i<30){ i++; print; }' out_JDATE_heliocentric.dat" u 3:4 w lp pt 2 lc 'green' t "2",\
  "<awk '!/^ #/ && (NR<15){ print 0,0,0,$5,$5,0,0; }' eclipses.dat | ./ellipses.awk" w l lw 0.5 lc 'gray' t "d_{max}",\
  "<awk '!/^ #/ && (NR<15){ print 0,0,0,$6,$6,0,0; }' eclipses.dat | ./ellipses.awk" w l lc 'black' t "d",\
  "<awk '!/^ #/ && (NR<15){ i++; print $1,$2,i; print $3,$4,i; print s; }' eclipses.dat" u 1:2:3 w lp pt 1 lw 2 lc palette z t "eclipses",\

pa -1


