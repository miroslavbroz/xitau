
f90 = gfortran
opt = -O3 -J../mod -fopenmp
opt = -O3 -J../mod
obj = \
	../misc/const.o \
	../misc/nrtype.o \
	../misc/nrutil.o \
	../shape/read_elem.o \
	../shape/read_face.o \
	../shape/read_node.o \
	../shape/write_elem.o \
	../shape/write_face.o \
	../shape/write_node.o \
	../multipole/read_multipole.o \
	../multipole/read_bruteforce.o \
	../multipole/vector_product.o \
	../multipole/volume.o \
	../multipole/surface.o \
	../multipole/centre.o \
	../multipole/inertia.o \
        ../multipole/spherical_cartesian.o \
	../multipole/legendre.o \
	../multipole/legendre2.o \
	../multipole/dipole.o \
	../multipole/factorial.o \
	../multipole/rotate.o \
	../multipole/multipole.o \
	../multipole/multipole2.o \
	../multipole/bruteforce.o \
	../multipole/jacobi.o \
	../multipole/normalize.o \
	../multipole/normal.o \
	../multipole/write_multipole.o \
	../multipole/write_bruteforce.o \
	../multipole/srtidx.o \
        ../multipole/getacc_bf.o \
        ../multipole/getacc_bf2.o \
        ../multipole/getacc_mp.o \
        ../multipole/getacc_mp2.o \
        dobrovolskis.o \

all: gravity

gravity: gravity.f90 $(obj)
	$(f90) $(obj) $(opt) -o $@ $<

$(obj): %.o:%.f90
	$(f90) $(opt) -c -o $@ $<

clean:
	rm -f *.mod
	rm $(obj)


