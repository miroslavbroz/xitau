#!/usr/bin/gnuplot

set term post eps enh color solid
set out "orbit1.eps"

set origin 0,0
set size 1,1
set multiplot
set size 1.0,0.33
set origin 0,0

set xl "JD - 2400000"
set yl "a [AU]"

p "<awk '($2==-2)' xvpl2el.out" u ($1-2400000):3 not w l lt 1

set origin 0,0.33
set yl "e []"

p "<awk '($2==-2)' xvpl2el.out" u ($1-2400000):4 not w l lt 1

set origin 0,0.66
set yl "i [deg]"

p "<awk '($2==-2)' xvpl2el.out" u ($1-2400000):5 not w l lt 1

unset multiplot
clear

########################################################################

set out "orbit2.eps"

set origin 0,0
set size 1,1
set multiplot
set size 1.0,0.33
set origin 0,0

set xl "JD - 2400000"
set yl "a [AU]"

p "<awk '($2==-3)' xvpl2el.out" u ($1-2400000):3 not w l lt 2

set origin 0,0.33
set yl "e []"

p "<awk '($2==-3)' xvpl2el.out" u ($1-2400000):4 not w l lt 2

set origin 0,0.66
set yl "i [deg]"

p "<awk '($2==-3)' xvpl2el.out" u ($1-2400000):5 not w l lt 2

unset multiplot
clear

########################################################################

set out "orbit3.eps"

set origin 0,0
set size 1,1
set multiplot
set size 1.0,0.33
set origin 0,0

set xl "JD - 2400000"
set yl "a [AU]"

p "<awk '($2==-4)' xvpl2el.out" u ($1-2400000):3 not w l lt 3

set origin 0,0.33
set yl "e []"

p "<awk '($2==-4)' xvpl2el.out" u ($1-2400000):4 not w l lt 3

set origin 0,0.66
set yl "i [deg]"

p "<awk '($2==-4)' xvpl2el.out" u ($1-2400000):5 not w l lt 3

unset multiplot
clear

########################################################################

set out "orbit4.eps"

set origin 0,0
set size 1,1
set multiplot
set size 1.0,0.33
set origin 0,0

set xl "JD - 2400000"
set yl "Omega [deg]"

p "<awk '($2==-2)' xvpl2el.out" u ($1-2400000):6 not w d lt 1

set origin 0,0.33
set yl "omega [deg]"

p "<awk '($2==-2)' xvpl2el.out" u ($1-2400000):7 not w d lt 1

set origin 0,0.66
set yl "M [deg]"

p "<awk '($2==-2)' xvpl2el.out" u ($1-2400000):8 not w d lt 1

unset multiplot
clear

########################################################################

set out "orbit5.eps"

set origin 0,0
set size 1,1
set multiplot
set size 1.0,0.33
set origin 0,0

set xl "JD - 2400000"
set yl "Omega [deg]"

p "<awk '($2==-3)' xvpl2el.out" u ($1-2400000):6 not w d lt 2

set origin 0,0.33
set yl "omega [deg]"

p "<awk '($2==-3)' xvpl2el.out" u ($1-2400000):7 not w d lt 2

set origin 0,0.66
set yl "M [deg]"

p "<awk '($2==-3)' xvpl2el.out" u ($1-2400000):8 not w d lt 2

unset multiplot
clear

########################################################################

set out "orbit6.eps"

set origin 0,0
set size 1,1
set multiplot
set size 1.0,0.33
set origin 0,0

set xl "JD - 2400000"
set yl "Omega [deg]"

p "<awk '($2==-4)' xvpl2el.out" u ($1-2400000):6 not w d lt 3

set origin 0,0.33
set yl "omega [deg]"

p "<awk '($2==-4)' xvpl2el.out" u ($1-2400000):7 not w d lt 3

set origin 0,0.66
set yl "M [deg]"

p "<awk '($2==-4)' xvpl2el.out" u ($1-2400000):8 not w d lt 3

unset multiplot
clear



