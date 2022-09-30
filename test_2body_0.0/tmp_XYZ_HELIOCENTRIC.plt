#!/usr/bin/gnuplot

set xl "x [AU]"
set yl "y [AU]"
set zl "z [AU]"
set size ratio -1

tmp=1.1
set xr [-tmp:tmp]
set yr [-tmp:tmp]
set zr [-tmp:tmp]
set zeroaxis
set nokey
set ticslevel 0
set view 0,0
set view equal xyz

sp \
  "<awk '($2==-1)' out_JDATE_heliocentric.dat" u 3:4:5 w d lw 2 lt 1,\
  "<awk '($2==-2)' out_JDATE_heliocentric.dat" u 3:4:5 w d lw 2 lt 2,\
  "<awk '($2==-3)' out_JDATE_heliocentric.dat" u 3:4:5 w d lw 2 lt 3,\
  "<awk '($2==-4)' out_JDATE_heliocentric.dat" u 3:4:5 w d lw 2 lt 4
pa -1

q

set term png small
set out "tmp_XYZ_HELIOCENTRIC.png"
rep

q
  "<awk '($2==-1) && ($1 >= 2456224.7247049999) && ($1 <= 2456224.7247049999+6)' out_JDATE_heliocentric.dat" u 3:4:5 w d lw 2 lt 1,\
  "<awk '($2==-2) && ($1 >= 2456224.7247049999) && ($1 <= 2456224.7247049999+6)' out_JDATE_heliocentric.dat" u 3:4:5 w d lw 2 lt 2,\
  "<awk '($2==-3) && ($1 >= 2456224.7247049999) && ($1 <= 2456224.7247049999+6)' out_JDATE_heliocentric.dat" u 3:4:5 w d lw 2 lt 3,\
  "<awk '($2==-4) && ($1 >= 2456224.7247049999) && ($1 <= 2456224.7247049999+6)' out_JDATE_heliocentric.dat" u 3:4:5 w d lw 2 lt 4

  "<awk '($2==-1) && ($1 == 2456224.7247049999)' out_JDATE_heliocentric.dat" u 3:4:5 w p lt 1,\
  "<awk '($2==-2) && ($1 == 2456224.7247049999)' out_JDATE_heliocentric.dat" u 3:4:5 w p lt 2,\
  "<awk '($2==-3) && ($1 == 2456224.7247049999)' out_JDATE_heliocentric.dat" u 3:4:5 w p lt 3,\
  "<awk '($2==-4) && ($1 == 2456224.7247049999)' out_JDATE_heliocentric.dat" u 3:4:5 w p lt 4,\

  "<awk '($2==-2) && ($1>=2446222.5)' out_JDATE_heliocentric.dat" u 3:4 w p lt 4 ps 0.5,\
  "<awk '($2==-3) && ($1>=2446222.5)' out_JDATE_heliocentric.dat" u 3:4 w p lt 8 ps 0.5,\
  "<echo 0 0" w p lt 2 ps 2,\
  "<./mind.awk -2 -3 out_JDATE_heliocentric.dat" u 1:2 w lp lt 1

