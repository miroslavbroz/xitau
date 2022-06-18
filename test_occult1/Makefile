
opt = -O3

obj = \
  ../misc/eps_earth.o \
  ../xvpl2el/nula2pi.o \
  ../sofa/tdbtt.o \
  ../sofa/tttai.o \
  ../sofa/taiut1.o \
  ../sofa/dtdb.o \
  ../sofa/era00.o \
  ../sofa/anp.o \
  ../test_sofa/gst06a.o \
  ../test_sofa/pnm06a.o \
  ../test_sofa/nut06a.o \
  ../test_sofa/fw2m.o \
  ../test_sofa/ir.o \
  ../test_sofa/rx.o \
  ../test_sofa/ry.o \
  ../test_sofa/rz.o \
  ../test_sofa/gst06.o \
  ../test_sofa/nut00a.o \
  ../test_sofa/pfw06.o \
  ../test_sofa/eors.o \
  ../test_sofa/fae03.o \
  ../test_sofa/faf03.o \
  ../test_sofa/faju03.o \
  ../test_sofa/fama03.o \
  ../test_sofa/fame03.o \
  ../test_sofa/faom03.o \
  ../test_sofa/fapa03.o \
  ../test_sofa/fasa03.o \
  ../test_sofa/faur03.o \
  ../test_sofa/fave03.o \
  ../test_sofa/obl06.o \
  ../test_sofa/s06.o \
  ../test_sofa/falp03.o \
  ../test_sofa/fad03.o \
  ../test_sofa/fal03.o \
  ../test_sofa/bpn2xy.o \

obj90 = \
  ../multipole/const.o \
  rotate.o \
  preces.o \
  nutate.o \
  intersect_AB_e.o \
  lst.o \
  hhms.o \
  geodetic.o \
  paralax.o \
  occult.o \

all: test_occult

test_occult: test_occult.f90 $(obj) $(obj90)
	gfortran $(opt) $(obj) $(obj90) -o $@ $<

$(obj): %.o:%.f
	gfortran $(opt) -c -o $@ $<

$(obj90): %.o:%.f90
	gfortran $(opt) -c -o $@ $<

clean:
	rm $(obj) $(obj90)

