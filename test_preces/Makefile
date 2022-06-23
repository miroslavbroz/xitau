
opt = -O3

obj = \
  ../misc/eps_earth.o \

obj90 = \
  ../multipole/const.o \
  ../occult/hhms.o \
  ../occult/rotateinv.o \
  ../occult/preces.o \
  ../occult/nutate.o \

all: test_preces

test_preces: test_preces.f90 $(obj) $(obj90)
	gfortran $(opt) $(obj) $(obj90) -o $@ $<

$(obj): %.o:%.f
	gfortran $(opt) -c -o $@ $<

$(obj90): %.o:%.f90
	gfortran $(opt) -c -o $@ $<

clean:
	rm $(obj) $(obj90)

