# Makefile for chi2el
# Miroslav Broz (miroslav.broz@email.cz), Jul 13th 2015

f77 = gfortran
opt = -O3

obj = \
  barycenters.o \
  e1.o \
  jacobian.o \
  jdate2bessel.o \
  nula2pi.o \
  read_chi2.o \
  write_elmts.o \
  vproduct.o \
  angmom.o \
  cartesian_spherical.o \
  rotat2.o \
  rotat3.o \

inc = \
  common.inc \
  ../misc/const.inc \
  ../chi2/chi2.inc \
  ../chi2/dependent.inc \

all : xv2el

xv2el : xv2el.f $(obj) $(inc)
	$(f77) $(opt) $(obj) -o $@ $<

$(obj) : %.o:%.f
	$(f77) $(opt) -c -o $@ $<

clean : FORCE
	rm $(obj)
FORCE :


