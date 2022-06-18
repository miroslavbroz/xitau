
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
  gst06a.o \
  pnm06a.o \
  nut06a.o \
  fw2m.o \
  ir.o \
  rx.o \
  ry.o \
  rz.o \
  gst06.o \
  nut00a.o \
  pfw06.o \
  eors.o \
  fae03.o \
  faf03.o \
  faju03.o \
  fama03.o \
  fame03.o \
  faom03.o \
  fapa03.o \
  fasa03.o \
  faur03.o \
  fave03.o \
  obl06.o \
  s06.o \
  falp03.o \
  fad03.o \
  fal03.o \
  bpn2xy.o \

obj90 = \
  ../test_occult1/hhms.o \

test_sofa: test_sofa.f90 $(obj) $(obj90)
	gfortran $(opt) $(obj) $(obj90) -o $@ $<

$(obj): %.o:%.f
	gfortran $(opt) -c -o $@ $<

$(obj90): %.o:%.f90
	gfortran $(opt) -c -o $@ $<

clean:
	rm $(obj)


