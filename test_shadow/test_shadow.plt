#!/usr/bin/gnuplot

set xl "u [arcsec]"
set yl "v [arcsec]"
set zl "w [arcsec]"

tmp=0.12
set xr [-tmp:tmp]
set yr [-tmp:tmp]
set zr [-tmp:tmp]
set zeroaxis
set xyplane 0.0
set view equal xyz
set view 0,0

sp \
  "<./face.awk nodes001.dat test_shadow.face" u 1:2:3 w l lc 'gray',\
  "test_shadow.silh" u 1:2:(0.0) w lp ps 2,\
  "<awk '($4==1)' nxyz.dat" u ($1*tmp):($2*tmp):($3*tmp) w lp lc 'blue'   lw 2,\
  "<awk '($4==2)' nxyz.dat" u ($1*tmp):($2*tmp):($3*tmp) w lp lc 'orange' lw 2,\

pa -1

q

  "<./face.awk nodes001.dat output.face" u 1:2:3 w l lc 'gray',\


