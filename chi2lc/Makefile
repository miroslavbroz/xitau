# Makefile
# Miroslav Broz (miroslav.broz@email.cz), Sep 6th 2015

f77 = gfortran

opt = -O3

lib = -L.

obj = \
  ../simplex/lum_func.o \
  ../simplex/luminosity_planck_bandpass.o \
  ../simplex/planck.o \
  ../simplex/qtrap.o \
  ../simplex/trapzd.o \

inc = \

all: chi2lc

chi2lc: chi2lc.f $(obj) $(inc)
	$(f77) $(opt) $(obj) -o $@ $< $(lib)

$(obj): %.o:%.f $(inc)
	$(f77) $(opt) -c -o $@ $<

clean: FORCE
	rm -f $(obj) $(objc)

FORCE:


