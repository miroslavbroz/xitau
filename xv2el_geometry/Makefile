# Makefile for chi2el
# Miroslav Broz (miroslav.broz@email.cz), Jul 13th 2015

f77 = gfortran
opt = -O3 -J../mod

obj = \
  nula2pi.o \
  coord/coord_b2j.o \
  orbel/orbel_xv2el.o \
  geometry/geometries.o \
  geometry/geometry_ecliptic.o \
  geometry/geometry_hierarch2.o \

obj90 = \
  read_chi2.o \
  ../main/read_dependent.o \

inc = \
  ../misc/const.inc \
  ../chi2/chi2.inc \
  ../chi2/dependent.inc \

all : xv2el

xv2el : xv2el.f $(obj) $(obj90) $(inc)
	$(f77) $(opt) $(obj) $(obj90) -o $@ $<

$(obj) : %.o:%.f
	$(f77) $(opt) -c -o $@ $<

$(obj90) : %.o:%.f90
	$(f77) $(opt) -c -o $@ $<

clean : FORCE
	rm $(obj) $(obj90)
FORCE :


