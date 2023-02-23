#!/usr/bin/gnuplot

f(x) = x>720. ? x-720. : x>360. ? x-360. : x
g(x) = f(x)>180. ? f(x)-360. : f(x)

au = 1.49597870700e11  # m
km = 1.e3  # m
R_S = 6.957e8  # m

set colors classic

set term png small
set out "orbit1.png"

set origin 0,0
set size 1,1
set multiplot
set size 1.0,0.33
set origin 0,0

load "T0.plt"
set arrow from (T0-2400000),graph 0 rto 0,graph 1 nohead lt 0 front
set arrow from (2454047.326750-2400000),graph 0 rto 0,graph 1 nohead lt 0 front
#`awk '!/^#/{ print "set arrow from " $1 "-2400000,graph 0 rto 0,graph 1 nohead lt 0 front;"; }' Sky2.dat`

set xl "JD - 2400000"
set yl "a [R_S]"
#set xr [T0-2400000:T0-2400000+20.]
set ytics format "%.5f"

p "<awk '($2==-2)' xvpl2el.out" u ($1-2400000):($3*au/R_S) not w l lt 2

set origin 0,0.33
set yl "e []"

p "<awk '($2==-2)' xvpl2el.out" u ($1-2400000):4 not w l lt 2

set origin 0,0.66
set yl "i [deg]"

p "<awk '($2==-2)' xvpl2el.out" u ($1-2400000):5 not w l lt 2,\

#  "<echo 2454047.326750 86.0" u ($1-2400000):2 w p ps 2 not,\
#  "<echo 2459579.344550 89.0" u ($1-2400000):2 w p ps 2 not,\

unset multiplot
clear

########################################################################

set out "orbit4.png"

set origin 0,0
set size 1,1
set multiplot
set size 1.0,0.33
set origin 0,0

set xl "JD - 2400000"
set yl "Omega [deg]"

p "<awk '($2==-2)' xvpl2el.out" u ($1-2400000):(g($6)) not w d lt 2

set origin 0,0.33
set yl "varpi [deg]"

p "<awk '($2==-2)' xvpl2el.out" u ($1-2400000):(g($6+$7)) not w d lt 2

set origin 0,0.66
set yl "lambda [deg]"

p "<awk '($2==-2)' xvpl2el.out" u ($1-2400000):(f($6+$7+$8)) not w d lt 2

unset multiplot
clear

########################################################################

set out "orbit2.png"

set origin 0,0
set size 1,1
set multiplot
set size 1.0,0.33
set origin 0,0

set xl "JD - 2400000"
set yl "a [R_S]"

set noarrow
set arrow from (T0-2400000),graph 0 rto 0,graph 1 nohead lt 0 front
`awk '!/^#/{ print "set arrow from " $1 "-2400000,graph 0 rto 0,graph 1 nohead lt 0 front;"; }' Sky3.dat`
 
p "<awk '($2==-3)' xvpl2el.out" u ($1-2400000):($3*au/R_S) not w l lt 3

set origin 0,0.33
set yl "e []"

p "<awk '($2==-3)' xvpl2el.out" u ($1-2400000):4 not w l lt 3

set origin 0,0.66
set yl "i [deg]"

p "<awk '($2==-3)' xvpl2el.out" u ($1-2400000):5 not w l lt 3

unset multiplot
clear

########################################################################

set out "orbit5.png"

set origin 0,0
set size 1,1
set multiplot
set size 1.0,0.33
set origin 0,0

set xl "JD - 2400000"
set yl "Omega [deg]"

p "<awk '($2==-3)' xvpl2el.out" u ($1-2400000):(g($6)) not w d lt 3

set origin 0,0.33
set yl "varpi [deg]"

p "<awk '($2==-3)' xvpl2el.out" u ($1-2400000):(g($6+$7)) not w d lt 3

set origin 0,0.66
set yl "lambda [deg]"

p "<awk '($2==-3)' xvpl2el.out" u ($1-2400000):(f($6+$7+$8)) not w d lt 3

unset multiplot
clear

########################################################################

set out "orbit3.png"

set origin 0,0
set size 1,1
set multiplot
set size 1.0,0.33
set origin 0,0

set xl "JD - 2400000"
set yl "a [R_S]"

p "<awk '($2==-4)' xvpl2el.out" u ($1-2400000):($3*au/R_S) not w l lt 4

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

p "<awk '($2==-4)' xvpl2el.out" u ($1-2400000):(g($6)) not w d lt 4

set origin 0,0.33
set yl "varpi [deg]"

p "<awk '($2==-4)' xvpl2el.out" u ($1-2400000):(g($6+$7)) not w d lt 4

set origin 0,0.66
set yl "lambda [deg]"

p "<awk '($2==-4)' xvpl2el.out" u ($1-2400000):(f($6+$7+$8)) not w d lt 4

unset multiplot
clear



