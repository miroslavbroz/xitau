
f77 = gfortran
opt = -O3

obj = \
  interp.o \
  limcof_read.o \
  limcof_interp.o \

inc = \
  limcof.inc

all: limcof_test

limcof_test: limcof_test.f $(obj) $(inc)
	$(f77) $(obj) $(opt) -o $@ $<

$(obj): %.o:%.f $(inc)
	$(f77) $(opt) -c $<

clean:
	rm $(obj)

