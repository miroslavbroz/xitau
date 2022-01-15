#!/usr/bin/gnuplot

set colors classic
set term post eps enh color dashed
set out "chi2_VIS.eps"
set size 1.0,0.9

set xl "{/Helvetica-Oblique B}/{/Symbol l} [cycles]"
set yl "{/Helvetica-Oblique V }^{2} [] (shifted by dataset number)"

set yr [:7]
set grid ytics mytics
set zeroaxis

fac=2.0

p \
  "Vis.dat"        u (sqrt($2**2+$3**2)/$4):($6+fac*$8-fac):7 t "observed" w err lt 3 pt 1 ps 0.5,\
  "visibility.dat" u (sqrt($2**2+$3**2)/$4):($5+fac*$6-fac)   t "synthetic" w p lc 'orange' pt 1,\
  "chi2_VIS.dat"   u (sqrt($2**2+$3**2)/$4):($6+fac*$8-fac)   t "residua" w l lt 1 lw 1,\
  "<awk '($NF+0>100)' chi2_VIS.dat" u (sqrt($2**2+$3**2)/$4):($6+fac*$8-fac) t "{/Symbol c}^2 > 100" w p lt 1 pt 6 ps 1.5

q



