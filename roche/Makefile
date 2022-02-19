
f90 = gfortran
opt = -O3

obj = \
  ../multipole/const.o \
  romberg.o \
  root.o \
  roche.o \

test_roche : test_roche.f90 $(obj)
	$(f90) $(opt) $(obj) -o $@ $<

$(obj) : %.o:%.f90
	$(f90) $(opt) -c -o $@ $<

clean :
	rm -f $(obj)


