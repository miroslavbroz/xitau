#!/usr/bin/env gnuplot

load "T0.plt"

JD0 = T0
P = x_param5
P = 7.147596
P = 7.143630

jd0 = T0+0.5*P
jd1 = T0+1.0*P
jd2 = T0+1.5*P
jd3 = T0+2.0*P
E = int((jd1-jd0)/P)
E = E+1
print "E = ", E

set colors classic
#set term x11

band = 7
band = 54
shift = 0.2
shift = 0.0

set xl "JD - 2400000"
set yl "magnitude [mag]"
set cbl "JD - 2400000"

load "T0.plt"

#tmp=5.0; set xr [T0-2400000-1.5:T0-2400000+tmp]
set yr [:] reverse
#set ytics shift
set grid ytics
set key right
set xtics format "%.3f"
set mouse format "%.6f"
set palette rgbformulae 33,13,10

set arrow from T0-2400000,graph 0 rto 0,graph 1 nohead lt 0 front
#set arrow from jd0-2400000,graph 0 rto 0,graph 1 nohead lt 0 front
#set arrow from jd1-2400000,graph 0 rto 0,graph 1 nohead lt 0 front
#set arrow from jd2-2400000,graph 0 rto 0,graph 1 nohead lt 0 front
#set arrow from jd3-2400000,graph 0 rto 0,graph 1 nohead lt 0 front

# from Omc12.dat_tdb:
jd=2456224.72474826; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2456228.30177255; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2456231.86899621; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2456235.44584543; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2458047.07954362; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2458050.65786909; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2458054.22769977; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2458057.80675461; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2459147.70172873; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2459151.27557915; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2459154.84385145; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2459161.98563354; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2459165.55942638; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2459169.12716193; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2459447.85610930; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2459451.42955560; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2459454.99786994; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2459458.57155293; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2459462.13937660; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2459465.71303021; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2459469.28151943; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2459472.85537704; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2459476.42368900; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2459479.99769962; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2459483.56664690; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2459490.71054897; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2459494.28539745; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2459497.85573637; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2459501.43079360; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2459505.00244509; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2459508.57847976; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2459515.72806984; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2459519.30153548; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2459522.87871038; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2460208.95771025; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2460212.52699399; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2460216.10066389; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2460219.67083647; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2460223.24550616; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2460226.81640264; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2460230.39164462; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2460237.53989389; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2460241.11295582; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2460244.68973424; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2460248.26348596; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2460251.84018159; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2460255.41348811; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0
jd=2460258.99035173; set arrow from jd-2400000,graph 0 rto 0,graph 1 nohead lt 0

set style arrow 1 size screen 0.03,90 heads lc 'green' lw 1 front

# from Ecl12.dat_tdb:
jd=2456224.72474826; d=0.265591; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2456228.30177255; d=0.260682; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2456231.86899621; d=0.268347; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2456235.44584543; d=0.261987; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2458047.07954362; d=0.257196; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2458050.65786909; d=0.249454; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2458054.22769977; d=0.258212; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2458057.80675461; d=0.260495; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2459147.70172873; d=0.254113; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2459151.27557915; d=0.255604; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2459154.84385145; d=0.255544; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2459161.98563354; d=0.255512; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2459165.55942638; d=0.258307; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2459169.12716193; d=0.255584; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2459447.85610930; d=0.258425; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2459451.42955560; d=0.255453; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2459454.99786994; d=0.255640; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2459462.13937660; d=0.255621; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2459465.71303021; d=0.255570; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2459469.28151943; d=0.255648; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2459472.85537704; d=0.252827; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2459476.42368900; d=0.255588; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2459479.99769962; d=0.255564; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2459483.56664690; d=0.255600; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2459490.71054897; d=0.255575; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2459494.28539745; d=0.252824; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2459497.85573637; d=0.255513; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2459501.43079360; d=0.252830; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2459505.00244509; d=0.252816; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2459508.57847976; d=0.255513; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2459515.72806984; d=0.255632; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2459519.30153548; d=0.255659; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2459522.87871038; d=0.256774; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2460208.95771025; d=0.252681; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2460212.52699399; d=0.249831; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2460216.10066389; d=0.250094; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2460219.67083647; d=0.248672; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2460223.24550616; d=0.250154; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2460226.81640264; d=0.250059; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2460230.39164462; d=0.252790; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2460237.53989389; d=0.250071; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2460241.11295582; d=0.252747; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2460244.68973424; d=0.252760; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2460248.26348596; d=0.252838; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2460251.84018159; d=0.252225; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2460255.41348811; d=0.255532; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1
jd=2460258.99035173; d=0.255698; set arrow from jd-2400000-d/2,1.0 to jd-2400000+d/2,1.0 as 1

p \
  "Lc.dat"         u ($1-2400000):2:($1-2400000) w l lc palette z not,\
  "Lc.dat"         u ($1-2400000):2:3 w err lt 3 pt 1 ps 0.5 t "observed",\
  "data/LC_most12.dat_tdb"    u ($1-2400000):2:3 w err lt 3 pt 1 ps 0.5 not,\
  "data/LC_most17.dat_tdb"    u ($1-2400000):2:3 w err lt 3 pt 1 ps 0.5 not,\
  "data/LC_tess31.dat_tdb"    u ($1-2400000):2:3 w err lt 3 pt 1 ps 0.5 not,\
  "data/LC_tess42.dat_tdb"    u ($1-2400000):2:3 w err lt 3 pt 1 ps 0.5 not,\
  "data/LC_tess43.dat_tdb"    u ($1-2400000):2:3 w err lt 3 pt 1 ps 0.5 not,\
  "data/LC_tess44.dat_tdb"    u ($1-2400000):2:3 w err lt 3 pt 1 ps 0.5 not,\
  "data/LC_tess70.dat_tdb"    u ($1-2400000):2:3 w err lt 3 pt 1 ps 0.5 not,\
  "data/LC_tess71.dat_tdb"    u ($1-2400000):2:3 w err lt 3 pt 1 ps 0.5 not,\
  "Lc_U.dat"       u ($1-2400000):($2+(5-band)*shift):3 t "U" w err lt 4 pt 1 ps 0.5,\
  "Lc_B.dat"       u ($1-2400000):($2+(6-band)*shift):3 t "B" w err lt 5 pt 1 ps 0.5,\
  "Lc_V.dat"       u ($1-2400000):($2+(7-band)*shift):3 t "V" w err lt 2 pt 1 ps 0.5,\
  "Lc_R.dat"       u ($1-2400000):($2+(8-band)*shift):3 t "R" w err lt 5 pt 1 ps 0.5,\
  "lightcurve.dat" u ($1-2400000):($2+($3-band)*shift) w lp pt 1 lc 'orange' t "synthetic",\
  "chi2_LC.dat"    u ($1-2400000):($2+($4-band)*shift):3 w l lt 1 lw 3 t "residua",\

pa -1

set term png small size 2048,1024
set out "chi2_LC.png"
rep

q


  "Lc_tess.dat"    u ($1-2400000):2:3 w l lt 3 not,\

