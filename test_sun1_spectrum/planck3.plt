#!/usr/bin/gnuplot

# planck.plt
# Planckuv zakon rho(omega,T) a souvisejici konstanty,
#   vcetne porovnani se spektrem Slunce.
# Miroslav Broz (miroslav.broz@email.cz), Oct 31st 2006

pi = 3.1415926535
h = 6.63e-34		# J.s
c = 299782458.		# m/s
k = 1.38e-23		# J/K

T_Sun = 5780.		# K, teplota Slunce
nm = 1.e-9

B(nu,T) = 2.*h*nu**3/c**2 * 1 / (exp(h*nu/(k*T)) - 1)

# Rayleighuv-Jeansuv zakon.

B_RJ(nu,T) = 2.*k*T/c**2 * nu**2

# Wienuv zakon.

B_Wien(nu,T) = 2.*h*nu**3/c**2 * exp(-h*nu/(k*T))

# nastaveni Gnuplotu

set xl "{/Symbol n} / Hz"
set yl "{/Helvetica-Oblique B}_{/Symbol n}({/Helvetica-Oblique T}) / W m^{{/Symbol -}2} Hz^{{/Symbol -}1} sr^{{/Symbol -}1}" offset -1,0
set x2l "{/Symbol l} / nm"

set samples 1000

set xr [1e-3:1.5e15]
set yr [0:4.e-8]
set xtics (\
  "0" 0,\
  "5{\264}10^{14}" 5e14,\
  "10^{15}" 1e15,\
  "1,5{\264}10^{15}" 1.5e15\
)
set xtics add (\
  "" 1e14 1,\
  "" 2e14 1,\
  "" 3e14 1,\
  "" 4e14 1,\
  "" 6e14 1,\
  "" 7e14 1,\
  "" 8e14 1,\
  "" 9e14 1,\
  "" 1.1e15 1,\
  "" 1.2e15 1,\
  "" 1.3e15 1,\
  "" 1.4e15 1\
)
set ytics (\
  "0" 0,\
  "10^{{/Symbol -}8}" 1e-8,\
  "2{\264}10^{{/Symbol -}8}" 2e-8,\
  "3{\264}10^{{/Symbol -}8}" 3e-8,\
  "4{\264}10^{{/Symbol -}8}" 4e-8\
)
set ytics add (\
  "" 0.5e-8 1,\
  "" 1.5e-8 1,\
  "" 2.5e-8 1,\
  "" 3.5e-8 1\
)
set xtics nomirror
f(lambda) = c/(lambda*nm)
set x2tics (\
  "200"  f( 200),\
  "300"  f( 300),\
  "400"  f( 400),\
  "500"  f( 500),\
  ""     f( 600) 1,\
  ""     f( 700) 1,\
  ""     f( 800) 1,\
  ""     f( 900) 1,\
  "1000" f(1000),\
  ""     f(1100) 1,\
  ""     f(1200) 1,\
  ""     f(1300) 1,\
  ""     f(1400) 1,\
  ""     f(1500) 1,\
  ""     f(1600) 1,\
  ""     f(1700) 1,\
  ""     f(1800) 1,\
  ""     f(1900) 1,\
  "2000" f(2000)\
)

lw=2
set style line  1 lt 1 lw 5
set style line  2 lt 2 lw 2
set style line  3 lt 4 lw 2
set style line  4 lt 5
set style line  5 lt 1 lw 1
set style line  6 lt 1 lw 0.25

set rmargin 3.3
set grid noxtics noytics front
tmp=6.5e14; set label "     {/Helvetica-BoldOblique T }{/Helvetica-Bold = 5780 K}" at tmp,B(tmp,T_Sun)

set label "{/=10 CaII}"          at f(852.), 2.8e-8 front center
set label "{/=10 H_{/Symbol a}}" at f(656.), 2.65e-8 front center
set label "{/=10 NaI}"           at f(589.), 2.65e-8 front center
set label "{/=10 MgI}"           at f(518.), 2.05e-8 front center
set label "{/=10 H_{/Symbol b}}" at f(486.), 1.85e-8 front center
set label "{/=10 CaII}"          at f(393.), 0.25e-8 front center
set label "{/=10 Fe}"            at f(427.), 1.15e-8 front center
set label "{/=10 Balmer}"        at f(330.), 0.90e-8 front center

set label "{/=10 .}" at f(434.), 1.85e-8 front center
#set label "{/=10 H_{/Symbol g}}" at f(434.), 1.85e-8 front center
#set label "{/=10 Fe}"            at f(527.), 1.50e-8 front center
#set label "{/=10 CaI}"           at f(423.), 1.50e-8 front center

g(x) = x < 3.9e-8 ? x : NaN
AU = 149.6e9	# m
R_S = 7.e8	# m

p B(x,T_Sun)       not w filledcurves ls 4,\
  B(x,T_Sun)       t "Planck"         ls 1,\
  g(B_RJ(x,T_Sun)) t "Rayleigh{\261}Jeans" ls 2,\
  B_Wien(x,T_Sun)  t "Wien"           ls 3,\
  "NewGuey2003.txt" u (c/($1*nm)):(($1*nm)**2/c * 1./(4*pi) * 1./nm * 1./(pi*R_S**2) * 4.*pi*AU**2 * $2)  t "{/=14 observed solar spectrum}" w l ls 6

set term post eps enh color dashed "Helvetica" 18
set out "planck3.eps"
set size 1.0,0.8
rep

q


tmp=2.0e14; set label "     {/=10 3000 K}" at tmp,B(tmp,3000.) front
tmp=2.5e14; set label "     {/=10 4000 K}" at tmp,B(tmp,4000.) front
tmp=3.3e14; set label "   {/=10 5000 K}" at tmp,B(tmp,5000.) front

#pa -1
#  B(x,50000.)      t "50000 K"        ls 5,\
  B(x,3000.)       not                ls 5,\
  B(x,4000.)       not                ls 5,\
  B(x,5000.)       not                ls 5,\

