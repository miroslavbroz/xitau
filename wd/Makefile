# Makefile
# Makefile for lc code.
# Miroslav Broz (miroslav.broz@email.cz), Sep 6th 2015

f77 = gfortran

opt = -O3

lib = -L.

obj = \
  atmx.o \
  bbl.o \
  cloud.o \
  conjph.o \
  dgmprd.o \
  dminv.o \
  dura.o \
  ellone.o \
  fourls.o \
  gabs.o \
  jdph.o \
  kepler.o \
  lc_call.o \
  lcr.o \
  legendre.o \
  light.o \
  linpro.o \
  lum.o \
  lump.o \
  mlrg.o \
  modlog.o \
  nekmin.o \
  olump.o \
  planckint.o \
  ranuni.o \
  ring.o \
  romq.o \
  sincos.o \
  spot.o \
  surfas.o \
  volume.o \

inc = \

all: lc lc_old

lc: lc.f $(obj) $(inc)
	$(f77) $(opt) $(obj) -o $@ $< $(lib)

lc_old: lc_old.f $(obj) $(inc)
	$(f77) $(opt) $(obj) -o $@ $< $(lib)

$(obj): %.o:%.f $(inc)
	$(f77) $(opt) -c -o $@ $<

clean: FORCE
	rm -f $(obj) $(objc)

FORCE:


