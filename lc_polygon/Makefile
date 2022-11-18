
opt = -O3 -J../mod

obj = \
  const.o \
  input.o \
  hapke.o \

test_hapke: test_hapke.f90 $(obj)
	gfortran $(opt) $(obj) -o $@ $<

$(obj): %.o:%.f90
	gfortran $(opt) -c $<

clean:
	rm $(obj)


