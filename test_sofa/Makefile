
opt = -O3

obj = \
  ../sofa/anp.o \
  ../sofa/cal2jd.o \
  ../sofa/dat.o \
  ../sofa/dtdb.o \
  ../sofa/era00.o \
  ../sofa/gmst06.o \
  ../sofa/jd2cal.o \
  ../sofa/taiut1.o \
  ../sofa/taitt.o \
  ../sofa/tdbtcb.o \
  ../sofa/tttcg.o \
  ../sofa/tttdb.o \
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
  ../occult/hhms.o \

test_sofa: test_sofa.f90 $(obj) $(obj90)
	gfortran $(opt) $(obj) $(obj90) -o $@ $<

$(obj): %.o:%.f
	gfortran $(opt) -c -o $@ $<

$(obj90): %.o:%.f90
	gfortran $(opt) -c -o $@ $<

clean:
	rm $(obj)


