
opt = -O3

obj = \
  ../misc/eps_earth.o \
  ../xvpl2el/nula2pi.o \
  ../sofa/anp.o \
  ../sofa/dat.o \
  ../sofa/cal2jd.o \
  ../sofa/jd2cal.o \
  ../sofa/dtdb.o \
  ../sofa/era00.o \
  ../sofa/taiutc.o \
  ../sofa/taiut1.o \
  ../sofa/tdbtt.o \
  ../sofa/tttai.o \
  ../sofa/utcut1.o \
  ../sofa/utctai.o \
  ../sofa/gst06a.o \
  ../sofa/pnm06a.o \
  ../sofa/nut06a.o \
  ../sofa/fw2m.o \
  ../sofa/ir.o \
  ../sofa/rx.o \
  ../sofa/ry.o \
  ../sofa/rz.o \
  ../sofa/gst06.o \
  ../sofa/nut00a.o \
  ../sofa/pfw06.o \
  ../sofa/eors.o \
  ../sofa/fae03.o \
  ../sofa/faf03.o \
  ../sofa/faju03.o \
  ../sofa/fama03.o \
  ../sofa/fame03.o \
  ../sofa/faom03.o \
  ../sofa/fapa03.o \
  ../sofa/fasa03.o \
  ../sofa/faur03.o \
  ../sofa/fave03.o \
  ../sofa/obl06.o \
  ../sofa/s06.o \
  ../sofa/falp03.o \
  ../sofa/fad03.o \
  ../sofa/fal03.o \
  ../sofa/bpn2xy.o \

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

