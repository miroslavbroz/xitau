
opt = -O3

obj = \
  ../misc/eps_earth.o \
  ../xvpl2el/nula2pi.o \

obj90 = \
  hhms.o \
  ../multipole/const.o \
  ../multipole/rotate.o \
  ../occult/lmst.o \
  ../occult/intersect_AB_k.o \
  ../occult/occult.o \

all: test_occult

test_occult: test_occult.f90 $(obj) $(obj90)
	gfortran $(opt) $(obj) $(obj90) -o $@ $<

$(obj): %.o:%.f
	gfortran $(opt) -c -o $@ $<

$(obj90): %.o:%.f90
	gfortran $(opt) -c -o $@ $<

clean:
	rm $(obj) $(obj90)

