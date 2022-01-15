#!/usr/bin/gnuplot

set colors classic

set term png small
set out "orbit1.png"

set origin 0,0
set size 1,1
set multiplot
set size 1.0,0.33
set origin 0,0

load "T0.plt"
set arrow from T0,graph 0 rto 0,graph 1 nohead lt 0 front

set xl "JD - 2400000"
set yl "a [AU]"

p "<awk '($2==-2)' xvpl2el.out" u ($1-2400000):3 not w l lt 2

set origin 0,0.33
set yl "e []"

p "<awk '($2==-2)' xvpl2el.out" u ($1-2400000):4 not w l lt 2

set origin 0,0.66
set yl "i [deg]"

p "<awk '($2==-2)' xvpl2el.out" u ($1-2400000):5 not w l lt 2

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

p "<awk '($2==-2)' xvpl2el.out" u ($1-2400000):6 not w d lt 2

set origin 0,0.33
set yl "omega [deg]"

p "<awk '($2==-2)' xvpl2el.out" u ($1-2400000):7 not w d lt 2

set origin 0,0.66
set yl "M [deg]"

p "<awk '($2==-2)' xvpl2el.out" u ($1-2400000):8 not w d lt 2

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
set yl "a [AU]"

p "<awk '($2==-3)' xvpl2el.out" u ($1-2400000):3 not w l lt 3

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

p "<awk '($2==-3)' xvpl2el.out" u ($1-2400000):6 not w d lt 3

set origin 0,0.33
set yl "omega [deg]"

p "<awk '($2==-3)' xvpl2el.out" u ($1-2400000):7 not w d lt 3

set origin 0,0.66
set yl "M [deg]"

p "<awk '($2==-3)' xvpl2el.out" u ($1-2400000):8 not w d lt 3

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
set yl "omega [deg]"

p "<awk '($2==-4)' xvpl2el.out" u ($1-2400000):7 not w d lt 4

set origin 0,0.66
set yl "M [deg]"

p "<awk '($2==-4)' xvpl2el.out" u ($1-2400000):8 not w d lt 4

unset multiplot
clear



