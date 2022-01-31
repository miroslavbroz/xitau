
f90=gfortran

opt=-O3

obj= \
  ../misc/eps_earth.o

test_eps: test_eps.f $(obj)
	$(f90) $(obj) $(opt) -o $@ $<

$(obj): %.o:%.f
	$(f90) $(opt) -c -o $@ $<

