# Makefile
# Makefile for simplex and chi^2.
# Miroslav Broz (miroslav.broz@email.cz), Sep 6th 2015

f77 = gfortran
f90 = gfortran
cc = g++

opt = -O3 -pg -mcmodel=large -Jmod 

lib = -L. -lstdc++

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
  misc/gammln.o \
  misc/gammp.o \
  misc/gammq.o \
  misc/gcf.o \
  misc/gser.o \
  misc/hermite.o \
  misc/integrate.o \
  misc/interp.o \
  misc/interp2.o \
  misc/kms_auday.o \
  misc/length.o \
  misc/lsm.o \
  misc/normalize.o \
  misc/quicksort.o \
  misc/qtrap.o \
  misc/ran1.o \
  misc/split.o \
  misc/srtidx.o \
  misc/trapzd.o \
  misc/vproduct.o \
  misc/omega_kopal.o \
  misc/omega_kopal_approx.o \
  misc/planck.o \
  misc/uvw.o \
  misc/uvw1.o \
  misc/uvw2.o \
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
  simplex/geometries.o \
  simplex/geometry_1centric.o \
  simplex/geometry_ecliptic.o \
  simplex/geometry_equatorial.o \
  simplex/geometry_hierarch.o \
  simplex/geometry_hierarch2.o \
  simplex/geometry_twopairs.o \
  simplex/geometry_twopairs2.o \
  simplex/geometry_periods.o \
  simplex/hec88.o \
  simplex/lum_func.o \
  simplex/luminosities.o \
  simplex/luminosity_planck_bandpass.o \
  simplex/luminosity_synthetic_bandpass.o \
  simplex/luminosity_synthetic_filters.o \
  simplex/merit_func.o \
  simplex/read_AO.o \
  simplex/read_CLO.o \
  simplex/read_ECL.o \
  simplex/read_LC.o \
  simplex/read_OCC.o \
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
  simplex/swift_bs_xyzb.o \
  simplex/write_poles.o \
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
  tides2/getacc_oblat.o \
  tides2/getacc_tides2.o \
  tu4/tu4_getaccb.o \
  tu4/tu4_getaccb_tp.o \
  util/util_exit.o \
  util/util_version.o \
  wd/atmx.o \
  wd/bbl_nbody.o \
  wd/binnum.o \
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
  wd/rangau.o \
  wd/ranuni.o \
  wd/ring.o \
  wd/romq.o \
  wd/sincos.o \
  wd/spot.o \
  wd/surfas.o \
  wd/volume.o \
  xvpl2el/nula2pi.o \
  sofa/cal2jd.o \
  sofa/dat.o \
  sofa/dtdb.o \
  sofa/jd2cal.o \
  sofa/taiut1.o \
  sofa/taiutc.o \
  sofa/tdbtt.o \
  sofa/tttai.o \
  sofa/utctai.o \
  sofa/utcut1.o \

# additional files if GST is needed!

#  sofa/gst06a.o \
#  sofa/anp.o \
#  sofa/era00.o \
#  sofa/pnm06a.o \
#  sofa/nut06a.o \
#  sofa/fw2m.o \
#  sofa/ir.o \
#  sofa/rx.o \
#  sofa/ry.o \
#  sofa/rz.o \
#  sofa/gst06.o \
#  sofa/nut00a.o \
#  sofa/pfw06.o \
#  sofa/eors.o \
#  sofa/fae03.o \
#  sofa/faf03.o \
#  sofa/faju03.o \
#  sofa/fama03.o \
#  sofa/fame03.o \
#  sofa/faom03.o \
#  sofa/fapa03.o \
#  sofa/fasa03.o \
#  sofa/faur03.o \
#  sofa/fave03.o \
#  sofa/obl06.o \
#  sofa/s06.o \
#  sofa/falp03.o \
#  sofa/fad03.o \
#  sofa/fal03.o \
#  sofa/bpn2xy.o \

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
  occult/intersect_AB_e.o \
  occult/intersect_AB_p.o \
  occult/rotateinv.o \
  occult/geodetic.o \
  occult/hhms.o \
  occult/lst.o \
  occult/nutate.o \
  occult/paralax.o \
  occult/preces.o \
  occult/occult.o \
  occult/fplane.o \
  occult/spath.o \
  occult/write_kml.o \
  lc_polygon/polytype.o \
  lc_polygon/input.o \
  lc_polygon/boundingbox.o \
  lc_polygon/clip.o \
  lc_polygon/hapke.o \
  lc_polygon/lambert.o \
  lc_polygon/lommel.o \
  lc_polygon/planck.o \
  lc_polygon/read_input.o \
  lc_polygon/surface.o \
  lc_polygon/to_poly.o \
  lc_polygon/to_three.o \
  lc_polygon/uvw.o \
  lc_polygon/write1.o \
  lc_polygon/write_poly.o \
  lc_polygon/centre_of_p.o \
  lc_polygon/normal_of_p.o \
  lc_polygon/shadowing_of_p.o \
  lc_polygon/rotate_of_p.o \
  lc_polygon/lc_polygon1.o \
  simplex/chi2_func_OCC.o \
  simplex/chi2_func_LC2.o \

objc = \
  lc_polygon/clip_in_c.o \
  clipper2/clipper.engine.o \

inc = \
  swift.inc \
  version.inc \
  filters/filters.inc \
  limcof/limcof.inc \
  misc/const.inc \
  simplex/dependent.inc \
  simplex/simplex.inc \
  simplex/cb_absol.inc \
  simplex/cb_itmax.inc \
  simplex/cb_limb.inc \
  simplex/cb_t3amp.inc \
  tides/spin.inc \
  tides/tides.inc \
  tides2/tides2.inc \
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

$(objc) : %.o:%.cpp $(inc)
	$(cc) $(opt) -c -o $@ $<

clean : FORCE
	rm -f mod/*.mod
	rm -f $(obj) $(obj90) $(objc)

FORCE :


