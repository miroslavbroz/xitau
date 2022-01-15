#!/usr/bin/gnuplot

chi2 = `awk 'BEGIN{ min=1e38; }{ chi=$NF; if (chi<min){ min=chi; } }END{ print min; }' chi2_func.tmp_1`

set table "m2_m3_min.tmp"
set dgrid3d 250,250,8
splot "chi2_func.tmp_1" u 2:3:55
unset table
unset dgrid3d

########################################################################

set term post eps enh color solid "Helvetica,18"
set out "m2_m3_min.eps"

set xl "{/Helvetica-Oblique m}_2 [10^{-16} {/Helvetica-Oblique M}_S]"
set yl "{/Helvetica-Oblique m}_3 [10^{-16} {/Helvetica-Oblique M}_S]"
#set zl "chi^2"
set cbl "{/Symbol c}^2"

set autoscale fix
#set xr [1.794:1.841]
#set yr [2.702:2.773]
tmp=1.8e2
set zr [tmp:]
set cbr [tmp:20000]
set logscale z
set logscale cb
#set xtics 0.01
#set ytics 0.01
set zeroaxis
set xyplane 0.0
set view 0,0
set nogrid
set isosamples 2
set pm3d map
#set hidden3d
#set nocolorbox
#tmp=6.5e4; set zr [0:tmp]; set cbr [0:tmp]
#set view 0,0,1.5; set view equal xy

#set palette rgbformulae 30,31,32
#set palette rgbformulae 33,13,10  # rainbow
set palette defined (\
  0.0  'blue',\
  0.2  'white',\
  1.0  '#ff9900' \
  )
set cbtics (\
  ""     4e2 1,\
  ""     5e2 1,\
  ""     6e2 1,\
  ""     7e2 1,\
  ""     8e2 1,\
  ""     9e2 1,\
  "10^3" 1e3,\
  ""     2e3 1,\
  ""     3e3 1,\
  ""     4e3 1,\
  ""     5e3 1,\
  ""     6e3 1,\
  ""     7e3 1,\
  ""     8e3 1,\
  ""     9e3 1,\
  "10^4" 1e4,\
  ""     2e4 1,\
  ""     3e4 1,\
  ""     4e4 1,\
  ""     5e4 1,\
  ""     6e4 1,\
  ""     7e4 1,\
  ""     8e4 1,\
  ""     9e4 1 \
  )

set lmargin 8.0
set rmargin 2.0
set bmargin 3.0
set tmargin 0.0

f(x) = x/1.e-16

sp \
  "m2_m3_min.tmp" u (f($1)):(f($2)):3 not w pm3d,\
  "<cat chi2_func.tmp_1" u (f($2)):(f($3)):55 not w p pt 1 lc 'gray',\
  "<awk 'BEGIN{ min=1e38; }{ chi=$NF; if (chi<min) { min=chi; s=$0; }}END{ print s; }' chi2_func.tmp_1" u (f($2)):(f($3)):55 not "best-fit" w p pt 6 ps 2 lw 3 lc 'red',\

system("patch m2_m3_min.eps m2_m3_min.eps.diff")

q

  "<awk '($NF<1000)' chi2_func.tmp_1" u 2:3:55:(sprintf("    %.0f",$55)) not w labels left font "Helvetica,8" tc 'white',\
  "<cat ../../216_fitting15_GRID*/chi2_func.tmp__315" u 2:3:53 t "nominal" w p pt 6 ps 3 lw 3 lc 'red',\
  "chi2_func.tmp_1" u 2:3:55:(sprintf("    %.0f",$55)) not w labels left font "Helvetica,8" tc 'black',\
  chi2 t sprintf("%.2f", chi2) w l lt 0


