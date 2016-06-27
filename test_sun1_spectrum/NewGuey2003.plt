#!/usr/bin/gnuplot

set xl "lambda / nm"
set yl "irradiance / W m^-2 nm^-1"

set xr [0:3000]

p "NewGuey2003.txt" u 1:2 w l
pa -1

########################################################################

nm = 1.e-9
c = 299782458.		# m/s

set xl "nu / Hz"
set yl "irradiance / W m^-2 Hz^-1 sr^-1"

p "NewGuey2003.txt" u (c/($1*nm)):(($1*nm)**2/c * $2/(4.*pi))
pa -1


