
f90 = gfortran
opt = -O3 -fopenmp
opt = -O3
obj = \
	read_elem.o \
	read_face.o \
	read_node.o \
	read_multipole.o \
	read_bruteforce.o \
	vector_product.o \
	volume.o \
	centre.o \
	inertia.o \
	legendre.o \
	dipole.o \
	factorial.o \
	multipole.o \
	const.o \
	bruteforce.o \
	rotate.o \
	nrtype.o \
	nrutil.o \
	jacobi.o \
	write_multipole.o \
        getacc_bf.o \
        getacc_bf2.o \
        getacc_mp.o \

test_getacc2: test_getacc2.f90 $(obj)
	$(f90) $(obj) $(opt) -I../multipole/ -o $@ $<

$(obj): %.o:%.f90
	$(f90) $(opt) -c -o $@ $<

clean:
	rm $(obj)


