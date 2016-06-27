
f77 = gfortran
opt = -O3

obj = \
  filter.o \
  integrate.o \
  interp.o \
  length.o \
  read_SED.o \
  read_synth.o \

test_vega: test_vega.f $(obj)
	$(f77) $(obj) $(opt) -o $@ $<

$(obj) : %.o:%.f
	$(f77) $(opt) -c -o $@ $<



