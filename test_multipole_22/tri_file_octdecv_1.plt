#!/usr/bin/gnuplot

set xl "x [km]"
set yl "y [km]"
set zl "z [km]"

tmp=150.
set xr [-tmp:tmp]
set yr [-tmp:tmp]
set zr [-tmp:tmp]

set view equal
set xyplane 0.0

sp "<./obj2plt.awk tri_file_octdecv_1.obj" u 1:2:3 w l

pa -1


