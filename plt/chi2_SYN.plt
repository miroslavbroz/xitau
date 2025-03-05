#!/usr/bin/gnuplot

set colors classic
#set terminal wxt font "Monospace,10"
#set term x11

c = 3.e8  # m s^-1
km = 1.e3  # m
nm = 1.e-9  # m
ang = 1.e-10/nm  # nm
shift = 0.125

set xl "lambda [nm]"
set yl "I_lambda [1] (shifted by dataset number)"

#tmp=5.; set xr [656.3-tmp:656.3+tmp]
#set xr [650.0:660.0]

#set ytics shift
#set mytics 1
#set grid ytics mytics
set zeroaxis
set bar 0.5
set key outside font "Helvetica,8" width -3
set tmargin 3.0

call "line.plt" "Halpha" 6562.81
call "line.plt" "Hbeta"  4861
call "line.plt" "Hgamma" 4341
#call "line.plt" "+19"    (4341*(1+19.*km/c))
call "line.plt" "-100"   (4341*(1-100.*km/c))
call "line.plt" "+100"   (4341*(1+100.*km/c))
call "line.plt" "-300"   (4341*(1-300.*km/c))
call "line.plt" "+300"   (4341*(1+300.*km/c))
call "line.plt" "Hdelta" 4102
call "line.plt" "Hepsil" 3970
call "line.plt" "HeI"    4009.258
call "line.plt" "HeI"    4026.210
call "line.plt" "HeI"    4120.811
call "line.plt" "HeI"    4143.761
call "line.plt" "HeI"    4387.929
call "line.plt" "HeI"    4471.498
call "line.plt" "HeI"    4713.139
call "line.plt" "HeI"    4921.931
call "line.plt" "HeI"    5015.678
call "line.plt" "HeI"    5047.738
call "line.plt" "HeI"    5875.625
call "line.plt" "HeI"    6678.154
call "line.plt" "CII"    4267
call "line.plt" "CII"    6578
call "line.plt" "CII"    6582
call "line.plt" "OII"    4649.143
call "line.plt" "MgII"   4481
call "line.plt" "SiII"   4128
call "line.plt" "SiII"   4130
call "line.plt" "SiII"   6347
call "line.plt" "SiII"   6371
call "line.plt" "NeI"    6402
call "line.plt" "FeIII"  5243.306

# from Walker etal. (2017), Tab. 5
call "line.plt" "NII"    6610.58
call "line.plt" "FeI"    6202.31
call "line.plt" "CIV"    5801.3
call "line.plt" "SiIII"  5739.7
call "line.plt" "CIII"   5695.0
call "line.plt" "OIII"   5592.3
call "line.plt" "NII"    4779.7
call "line.plt" "HeII"   4685.7
call "line.plt" "HeII"   5411.5

# from Shenar etal. (2015), Sec. 3.2
call "line.plt" "HeII"   4200
call "line.plt" "HeII"   4542
call "line.plt" "HeII"   6683

# from Greenstein & Aller (1950)
call "line.plt" "DIB"    4430
call "line.plt" "DIB"    6284

# Harmanec (pers. comm.)
#call "line.plt" "MgII"   4384.637  # weak
#call "line.plt" "MgII"   4390.514  # weak
#call "line.plt" "MgII"   4390.572  # weak
#call "line.plt" "FeIII"  4382.511  # weak

# unknown l.
call "line.plt" "?" 4304
call "line.plt" "?" 4318
call "line.plt" "?" 4350
call "line.plt" "?" 4367
call "line.plt" "?" 4379
call "line.plt" "?" 4415
call "line.plt" "?" 4437
call "line.plt" "?" 4447

x0=430.0
x0=446.0
#x0=475.0
x0=650.0

p \
  "synthetic.dat" u ($2/nm):($3+shift*($4-1))   t "synthetic" w l lc 'orange',\
  "Spectra.dat"   u ($2/nm):($3+shift*($5-1)):4 t "observed" w err lt 3 pt 1 ps 0,\
  "chi2_SYN.dat"  u ($2/nm):($3+shift*($5-1))   t "residua"  w l lt 1 lw 1,\
  "<awk '($NF+0>100)' chi2_SYN.dat" u ($2/nm):($3+shift*($5-1)) t "chi^2 > 100" w p lt 1 pt 6 ps 1.5,\
  "<awk '($5!=l){ print; }{ l=$5; }' Spectra.dat" u (x0):(1.0-0.05+shift*($5-1)):5 w labels left not
pa -1

set term png small size 2048,1536
set out "chi2_SYN.png"
rep

q

  "<awk '($NF+0>100)' chi2_VIS.dat" u (sqrt($2**2+$3**2)/$4):($5+$7) t "chi^2 > 100" w p lt 1 pt 6 ps 1.5

