
f77 = gfortran
opt = -O3

obj = \
  planck.o \

test_planck: test_planck.f $(obj)
	$(f77) $(obj) $(opt) -o $@ $<

$(obj) : %.o:%.f
	$(f77) $(opt) -c -o $@ $<



