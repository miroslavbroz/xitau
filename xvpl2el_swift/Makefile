# Makefile for chi2el
# Miroslav Broz (miroslav.broz@email.cz), Jul 13th 2015

f77 = gfortran
opt = -O3

obj = \
  read_chi2.o \
  write_elmts.o \
  nula2pi.o \
  jdate2bessel.o \
  coord/coord_h2j.o \
  orbel/orbel_xv2el.o \

inc = \
  common.inc \
  ../misc/const.inc \
  ../simplex/simplex.inc \
  ../simplex/dependent.inc \

all : xvpl2el

xvpl2el : xvpl2el.f $(obj) $(inc)
	$(f77) $(opt) $(obj) -o $@ $<

$(obj) : %.o:%.f
	$(f77) $(opt) -c -o $@ $<

clean : FORCE
	rm $(obj)
FORCE :


