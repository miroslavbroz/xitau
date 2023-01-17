
opt = -O3 -J../mod

obj = \
  ../misc/const.o \
  ../lc_polygon/input.o \
  ../lc_polygon/hapke.o \

test_hapke: test_hapke.f90 $(obj)
	gfortran $(opt) $(obj) -o $@ $<

$(obj): %.o:%.f90
	gfortran $(opt) -c $<

clean:
	rm $(obj)


