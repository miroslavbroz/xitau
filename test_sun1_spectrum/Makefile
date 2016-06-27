
f77 = gfortran
opt = -O3

obj = \
  integrate.o \
  interp.o \
  length.o \
  read_SED.o \
  read_synth.o \

test_sun: test_sun.f $(obj)
	$(f77) $(obj) $(opt) -o $@ $<

$(obj) : %.o:%.f
	$(f77) $(opt) -c -o $@ $<

clean :
	rm $(obj)


