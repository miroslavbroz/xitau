#!/usr/bin/gnuplot

day = 86400.  # s
c = 299792458.  # m/s, from CODATA 2018, exact
au = 1.49597870700e11  # m, from IAU 2012

# Mayer et al., Eq. (1)
P1 = 5.998570
jd0 = 2441033.033

# Mayer et al. 
P1 = 5.998693
jd0 = 2449425.099

# chi2.in, osculating
P1 = 5.9985999098751694
jd0 = 2435589.3105465779

# chi2.in, mean (adjusted)
P1 = 5.998680
jd0 = 2435589.3105465779

frac(x) = x-int(x)
g(x) = x < 0.5 ? x : x-1.0
OMC(jd) = g(frac((jd-jd0)/P1))*P1
OMCs(jd) = g(frac((jd-jd0-0.5*P1)/P1))*P1

#set colors classic

set xl "JD {/Symbol -} 2400000"
set yl "ETV [day] wrt. 2-body model" offset +0.5,0

tmp = 0.25
set yr [-tmp:tmp]
set tics front
set key
set samples 2000
set tmargin 1.5

load "T0.plt"
set arrow from graph 0,first 0 rto graph 1,first 0 nohead lt 0 front
set arrow from T0-2400000,graph 0 rto 0,graph 1 nohead lt 0 front
set label "{/=12{/Helvetica-Oblique T}_0}" at T0-2400000,graph 1.035 center

offset = 0.152

p \
  "<awk '($2==1)' minima.dat" u ($1-2400000):(OMC($1)) t "synthetic primary minima (no LITE)" w lp lc 'green' pt 1 ps 1,\
  "<awk '($2==1)' minima.dat" u ($1-2400000):($3+$4+offset) t "LITE" w lp lc '#00aa00' pt 1 ps 1,\
  "Omc12.dat" u ($1-2400000):3 w p t "Mayer et al., Fig. 9" pt 4 lc 'gray',\

pa -1

set term png small
set out "minima.png"
rep

q


  "chi2_TTV.dat" u ($1-2400000):(OMC($1)+offset) w p lc 'black',\
  "chi2_TTV.dat" u ($1-2400000):(OMC($2)+offset) w p lc 'green',\
  "chi2_TTV.dat" u ($1-2400000):3 w p lc 'red',\

  "<awk '($2==2)' minima.dat" u ($1-2400000):(OMCs($1)) t "synthetic secondary minima (no LITE)" w lp lc 'cyan' pt 1 ps 1,\
  "<awk '($2==1)' minima.dat" u ($1-2400000):($4) t "LITE12" w lp lc '#006600' pt 1 ps 1,\

  "<awk '($2==-1)' out_JDATE_barycentric.dat" u ($1-2400000):($5*au/c/day) w l lt 1 t "lite 1",\
  "<awk '($2==-2)' out_JDATE_barycentric.dat" u ($1-2400000):($5*au/c/day) w l lt 2 t "lite 2",\
  "<awk '($2==-3)' out_JDATE_barycentric.dat" u ($1-2400000):($5*au/c/day) w l lt 3 t "lite 3",\


