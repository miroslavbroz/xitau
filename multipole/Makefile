
f90 = gfortran
opt = -O3 -J../mod -fopenmp
opt = -O3 -J../mod
obj = \
	const.o \
	read_elem.o \
	read_face.o \
	read_node.o \
	read_multipole.o \
	read_bruteforce.o \
	vector_product.o \
	volume.o \
	surface.o \
	centre.o \
	inertia.o \
        spherical_cartesian.o \
	legendre.o \
	legendre2.o \
	dipole.o \
	factorial.o \
	multipole.o \
	multipole2.o \
	bruteforce.o \
	rotate.o \
	nrtype.o \
	nrutil.o \
	jacobi.o \
	normalize.o \
	normal.o \
	write_multipole.o \
	write_bruteforce.o \
	write_elem.o \
	write_face.o \
	write_node.o \
	srtidx.o \
        getacc_bf.o \
        getacc_bf2.o \
        getacc_mp.o \
        getacc_mp2.o \

all: gravity test_getacc test_legendre test_rotate

gravity: gravity.f90 $(obj)
	$(f90) $(obj) $(opt) -o $@ $<

test_getacc: test_getacc.f90 $(obj)
	$(f90) $(obj) $(opt) -o $@ $<

test_legendre: test_legendre.f90 $(obj)
	$(f90) $(obj) $(opt) -o $@ $<

test_rotate: test_rotate.f90 $(obj)
	$(f90) $(obj) $(opt) -o $@ $<

$(obj): %.o:%.f90
	$(f90) $(opt) -c -o $@ $<

clean:
	rm -f *.mod
	rm $(obj)


