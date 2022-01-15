#!/usr/bin/gnuplot

km = 1.e3
au = 1.49597870700e11

f(x) = x>720. ? x-720. : x>360. ? x-360. : x

set colors classic

set term post eps enh color solid "Helvetica,18"
set out "orbit1.eps"

set origin 0,0
set size 1,1
set multiplot
set size 1.0,0.40
set origin 0,0

load "T0.plt"
set arrow from (T0-2400000),graph 0 rto 0,graph 1 nohead lt 0 front

set xl "{/Helvetica-Oblique JD} - 2400000"
set yl "{/Helvetica-Oblique a}_1 [km]" offset +0,0
set xr [:58750]
set ytics 2
set lmargin 8.0
set rmargin 0.5

p "<awk '($2==-2)' xvpl2el.out" u ($1-2400000):($3*au/km) not w l lt 2

set size 1.0,0.34
set origin 0,0.37
set xl ""
set yl "{/Helvetica-Oblique e}_1 []" offset +1,0
set format x ""
set ytics 0.01

p "<awk '($2==-2)' xvpl2el.out" u ($1-2400000):4 not w l lt 2

set origin 0,0.68
set yl "{/Helvetica-Oblique i}_1 [deg]" offset +1,0
set ytics auto

p "<awk '($2==-2)' xvpl2el.out" u ($1-2400000):5 not w l lt 2

unset multiplot
clear

########################################################################

set out "orbit4.eps"

set origin 0,0
set size 1,1
set multiplot
set size 1.0,0.40
set origin 0,0

set xl "{/Helvetica-Oblique JD} - 2400000"
set yl "{/Symbol W}_1 [deg]" offset +2,0
set ytics 0.10

p "<awk '($2==-2)' xvpl2el.out" u ($1-2400000):6 not w d lt 2

set size 1.0,0.34
set origin 0,0.37
set xl ""
set yl "{/Symbol w}_1 [deg]" offset 0,0
set yr [0:360]
set ytics 60

p "<awk '($2==-2)' xvpl2el.out" u ($1-2400000):(f($6+$7)) not w d lt 2

set origin 0,0.68
set yl "{/Symbol l}_1 [deg]" offset 0,0

p "<awk '($2==-2)' xvpl2el.out" u ($1-2400000):(f($6+$7+$8)) not w d lt 2

unset multiplot
clear

########################################################################

set out "orbit2.eps"

set origin 0,0
set size 1,1
set multiplot
set size 1.0,0.40
set origin 0,0

set xl "{/Helvetica-Oblique JD} - 2400000"
set yl "{/Helvetica-Oblique a}_2 [km]"
set format x "% h"
set xtics auto
set autoscale y
set ytics 2

p "<awk '($2==-3)' xvpl2el.out" u ($1-2400000):($3*au/km) not w l lt 3

set size 1.0,0.34
set origin 0,0.37
set xl ""
set yl "{/Helvetica-Oblique e}_2 []" offset 1,0
set format x ""
set ytics 0.01

p "<awk '($2==-3)' xvpl2el.out" u ($1-2400000):4 not w l lt 3

set origin 0,0.68
set yl "{/Helvetica-Oblique i}_2 [deg]" offset 1,0
set ytics 0.1

p "<awk '($2==-3)' xvpl2el.out" u ($1-2400000):5 not w l lt 3

unset multiplot
clear

########################################################################

set out "orbit5.eps"

set origin 0,0
set size 1,1
set multiplot
set size 1.0,0.33
set origin 0,0

set xl "{/Helvetica-Oblique JD} - 2400000"
set yl "{/Helvetica-Oblique O}_2 [deg]"

p "<awk '($2==-3)' xvpl2el.out" u ($1-2400000):6 not w d lt 3

set origin 0,0.33
set yl "{/Helvetica-Oblique o}_2 [deg]"

p "<awk '($2==-3)' xvpl2el.out" u ($1-2400000):(f($6+$7)) not w d lt 3

set origin 0,0.66
set yl "{/Helvetica-Oblique l}_2 [deg]"

p "<awk '($2==-3)' xvpl2el.out" u ($1-2400000):(f($6+$7+$8)) not w d lt 3

unset multiplot
clear

q

########################################################################

set out "orbit3.png"

set origin 0,0
set size 1,1
set multiplot
set size 1.0,0.33
set origin 0,0

set xl "JD - 2400000"
set yl "a [AU]"

p "<awk '($2==-4)' xvpl2el.out" u ($1-2400000):3 not w l lt 4

set origin 0,0.33
set yl "e []"

p "<awk '($2==-4)' xvpl2el.out" u ($1-2400000):4 not w l lt 4

set origin 0,0.66
set yl "i [deg]"

p "<awk '($2==-4)' xvpl2el.out" u ($1-2400000):5 not w l lt 4

unset multiplot
clear

########################################################################

set out "orbit6.png"

set origin 0,0
set size 1,1
set multiplot
set size 1.0,0.33
set origin 0,0

set xl "JD - 2400000"
set yl "Omega [deg]"

p "<awk '($2==-4)' xvpl2el.out" u ($1-2400000):6 not w d lt 4

set origin 0,0.33
set yl "lambda [deg]"

p "<awk '($2==-4)' xvpl2el.out" u ($1-2400000):(f($6+$7)) not w d lt 4

set origin 0,0.66
set yl "lambda [deg]"

p "<awk '($2==-4)' xvpl2el.out" u ($1-2400000):(f($6+$7+$8)) not w d lt 4

unset multiplot
clear



