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
  uvw2.o \
  vproduct.o \
  write_elmts.o \

inc = \
  common.inc \
  ../misc/const.inc \
  ../chi2/chi2.inc \
  ../chi2/dependent.inc \

all : xvpl2el

xvpl2el : xvpl2el.f $(obj) $(inc)
	$(f77) $(opt) $(obj) -o $@ $<

$(obj) : %.o:%.f
	$(f77) $(opt) -c -o $@ $<

clean : FORCE
	rm $(obj)
FORCE :


