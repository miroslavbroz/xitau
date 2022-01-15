
f90 = gfortran
#opt = -O3 -fopenmp
opt = -O3

obj = \
	../multipole/const.o \
	../multipole/read_elem.o \
	../multipole/read_face.o \
	../multipole/read_node.o \
	../multipole/vector_product.o \
	../multipole/normalize.o \
	../multipole/normal.o \
	../multipole/write_face.o \
	read_pnm.o \
	write_pnm.o \
	write_silh.o \
        shadowing.o \
        silhouette.o \

all: test_silh test_shadow

test_silh: test_silh.f90 $(obj)
	$(f90) $(obj) $(opt) -o $@ $<

test_shadow: test_shadow.f90 $(obj)
	$(f90) $(obj) $(opt) -o $@ $<

$(obj): %.o:%.f90
	$(f90) $(opt) -c -o $@ $<

clean:
	rm $(obj)


