# Makefile
# Makefile for simplex and chi^2.
# Miroslav Broz (miroslav.broz@email.cz), Sep 6th 2015

f77 = gfortran
f90 = gfortran
cc = gcc

opt = -O3 -pg

lib = -L.

obj = \
  anal/anal_energy.o \
  anal/anal_energy_write.o \
  bs/bs_der.o \
  bs/bs_int.o \
  bs/bs_step.o \
  coord/coord_h2b.o \
  coord/coord_h2j.o \
  coord/coord_j2b.o \
  io/io_dump_param.o \
  io/io_dump_pl.o \
  io/io_energy_write.o \
  io/io_init_param.o \
  io/io_init_pl.o \
  io/io_open.o \
  io/io_write_pl.o \
  limcof/limcof_read.o \
  limcof/limcof_interp.o \
  main/read_dependent.o \
  main/read_dependent_bs.o \
  misc/arcsec_au.o \
  misc/au_arcsec.o \
  misc/au_day.o \
  misc/auday_kms.o \
  misc/dotprod.o \
  misc/eps_earth.o \
  misc/kms_auday.o \
  misc/length.o \
  misc/normalize.o \
  misc/quicksort.o \
  misc/split.o \
  misc/vproduct.o \
  mvs/drift/drift_dan.o \
  mvs/drift/drift_kepmd.o \
  mvs/drift/drift_kepu.o \
  mvs/drift/drift_kepu_fchk.o \
  mvs/drift/drift_kepu_guess.o \
  mvs/drift/drift_kepu_lag.o \
  mvs/drift/drift_kepu_new.o \
  mvs/drift/drift_kepu_p3solve.o \
  mvs/drift/drift_kepu_stumpff.o \
  mvs/drift/drift_one.o \
  obl/obl_acc.o \
  obl/obl_acc_tp.o \
  orbel/orbel_eget.o \
  orbel/orbel_ehie.o \
  orbel/orbel_ehybrid.o \
  orbel/orbel_el2xv.o \
  orbel/orbel_esolmd.o \
  orbel/orbel_fget.o \
  orbel/orbel_fhybrid.o \
  orbel/orbel_flon.o \
  orbel/orbel_scget.o \
  orbel/orbel_schget.o \
  orbel/orbel_xv2el.o \
  orbel/orbel_zget.o \
  ppn/getacc_ppn.o \
  pyterpol/pyterpol_.o \
  simplex/amebsa.o \
  simplex/amoeba.o \
  simplex/amotry.o \
  simplex/amotsa.o \
  simplex/baryc.o \
  simplex/bessj1.o \
  simplex/bessj32.o \
  simplex/chi2_func.o \
  simplex/chi2_func_AO.o \
  simplex/chi2_func_CLO.o \
  simplex/chi2_func_ECL.o \
  simplex/chi2_func_LC.o \
  simplex/chi2_func_RV.o \
  simplex/chi2_func_SED.o \
  simplex/chi2_func_SKY.o \
  simplex/chi2_func_SKY2.o \
  simplex/chi2_func_SKY3.o \
  simplex/chi2_func_SYN.o \
  simplex/chi2_func_T3.o \
  simplex/chi2_func_TTV.o \
  simplex/chi2_func_VIS.o \
  simplex/distance_AB_C.o \
  simplex/filter.o \
  simplex/gammln.o \
  simplex/gammp.o \
  simplex/gammq.o \
  simplex/gcf.o \
  simplex/geometries.o \
  simplex/geometry_1centric.o \
  simplex/geometry_ecliptic.o \
  simplex/geometry_equatorial.o \
  simplex/geometry_hierarch.o \
  simplex/geometry_hierarch2.o \
  simplex/geometry_twopairs.o \
  simplex/geometry_twopairs2.o \
  simplex/gser.o \
  simplex/hec88.o \
  simplex/hermite.o \
  simplex/interp.o \
  simplex/integrate.o \
  simplex/lsm.o \
  simplex/lum_func.o \
  simplex/luminosities.o \
  simplex/luminosity_planck_bandpass.o \
  simplex/luminosity_synthetic_bandpass.o \
  simplex/luminosity_synthetic_filters.o \
  simplex/merit_func.o \
  simplex/omega_kopal.o \
  simplex/omega_kopal_approx.o \
  simplex/planck.o \
  simplex/qtrap.o \
  simplex/ran1.o \
  simplex/read_AO.o \
  simplex/read_CLO.o \
  simplex/read_ECL.o \
  simplex/read_LC.o \
  simplex/read_RV.o \
  simplex/read_SED.o \
  simplex/read_SKY.o \
  simplex/read_SKY2.o \
  simplex/read_SYN.o \
  simplex/read_TTV.o \
  simplex/read_VIS.o \
  simplex/read_ephemeris.o \
  simplex/read_filter.o \
  simplex/read_synth.o \
  simplex/read_time.o \
  simplex/read_time_all.o \
  simplex/srtidx.o \
  simplex/swift_bs_xyzb.o \
  simplex/trapzd.o \
  simplex/uvw.o \
  simplex/uvw1.o \
  simplex/uvw2.o \
  simplex/write_uvw.o \
  subplex/calcc.o \
  subplex/dasum.o \
  subplex/daxpy.o \
  subplex/dcopy.o \
  subplex/dist.o \
  subplex/dscal.o \
  subplex/evalf.o \
  subplex/fstats.o \
  subplex/newpt.o \
  subplex/order.o \
  subplex/partx.o \
  subplex/setstp.o \
  subplex/simplx.o \
  subplex/sortd.o \
  subplex/start.o \
  subplex/subopt.o \
  subplex/subplx.o \
  tides/dissipation_factor.o \
  tides/getaccb_tides.o \
  tides/io_dump_spin.o \
  tides/io_init_spin.o \
  tides/io_init_tides.o \
  tides/io_write_spin.o \
  tides/mignard.o \
  tides/mignard_torque.o \
  tides/spin_evolve.o \
  tides2/io_init_tides2.o \
  tides2/getacc_tides2.o \
  tu4/tu4_getaccb.o \
  tu4/tu4_getaccb_tp.o \
  util/util_exit.o \
  util/util_version.o \
  wd/atmx.o \
  wd/bbl_nbody.o \
  wd/cloud.o \
  wd/conjph.o \
  wd/dgmprd.o \
  wd/dminv.o \
  wd/dura.o \
  wd/ellone.o \
  wd/fourls.o \
  wd/gabs.o \
  wd/jdph.o \
  wd/kepler.o \
  wd/lcr.o \
  wd/lc_call_nbody.o \
  wd/lc_read_in.o \
  wd/legendre.o \
  wd/light.o \
  wd/linpro.o \
  wd/lum.o \
  wd/lump.o \
  wd/mlrg.o \
  wd/modlog.o \
  wd/nekmin.o \
  wd/olump.o \
  wd/planckint.o \
  wd/ranuni.o \
  wd/ring.o \
  wd/romq.o \
  wd/sincos.o \
  wd/spot.o \
  wd/surfas.o \
  wd/volume.o \
  xvpl2el/nula2pi.o \

obj90 = \
  multipole/const.o \
  ao/read_pnm.o \
  ao/write_pnm.o \
  ao/write_silh.o \
  ao/shadowing.o \
  ao/silhouette.o \
  multipole/read_elem.o \
  multipole/read_face.o \
  multipole/read_node.o \
  multipole/read_bruteforce.o \
  multipole/read_multipole.o \
  multipole/vector_product.o \
  multipole/volume.o \
  multipole/centre.o \
  multipole/inertia.o \
  multipole/spherical_cartesian.o \
  multipole/legendre.o \
  multipole/legendre2.o \
  multipole/dipole.o \
  multipole/factorial.o \
  multipole/multipole.o \
  multipole/multipole2.o \
  multipole/bruteforce.o \
  multipole/rotate.o \
  multipole/nrtype.o \
  multipole/nrutil.o \
  multipole/jacobi.o \
  multipole/normalize.o \
  multipole/normal.o \
  multipole/write_elem.o \
  multipole/write_face.o \
  multipole/write_node.o \
  multipole/write_multipole.o \
  multipole/getacc_bf.o \
  multipole/getacc_bf2.o \
  multipole/getacc_mp.o \
  multipole/getacc_mp2.o \
  roche/root.o \
  roche/romberg.o \
  roche/roche.o \
  roche/omega_roche_approx.o \
  simplex/uvw_nodes.o \

objc = \

inc = \
  swift.inc \
  version.inc \
  limcof/limcof.inc \
  misc/const.inc \
  simplex/dependent.inc \
  simplex/filters.inc \
  simplex/simplex.inc \
  simplex/cb_absol.inc \
  simplex/cb_itmax.inc \
  simplex/cb_limb.inc \
  simplex/cb_t3amp.inc \
  tides/tides.inc \
  wd/lc.inc \
  subplex/usubc.inc \

all: main/chi2 main/simplex main/simann main/subplex main/swift_bs

main/chi2: main/chi2.f $(obj90) $(obj) $(objc) $(inc)
	$(f77) $(opt) $(obj) $(obj90) $(objc) -o $@ $< $(lib)

main/simplex: main/simplex.f $(obj90) $(obj) $(objc) $(inc)
	$(f77) $(opt) $(obj) $(obj90) $(objc) -o $@ $< $(lib)

main/simann: main/simann.f $(obj90) $(obj) $(objc) $(inc)
	$(f77) $(opt) $(obj) $(obj90) $(objc) -o $@ $< $(lib)

main/subplex: main/subplex.f $(obj90) $(obj) $(objc) $(inc)
	$(f77) $(opt) $(obj) $(obj90) $(objc) -o $@ $< $(lib)

main/swift_bs: main/swift_bs.f $(obj90) $(obj) $(objc) $(inc)
	$(f77) $(opt) $(obj) $(obj90) $(objc) -o $@ $< $(lib)

$(obj90) : %.o:%.f90 $(inc)
	$(f90) $(opt) -c -o $@ $<

$(obj) : %.o:%.f $(inc)
	$(f77) $(opt) -c -o $@ $<

$(objc) : %.o:%.c $(inc)
	$(cc) $(opt) -c -o $@ $<

clean : FORCE
	rm -f $(obj) $(obj90) $(objc)

FORCE :


