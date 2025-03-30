# Makefile
# Makefile for simplex and chi^2.
# Miroslav Broz (miroslav.broz@email.cz), Sep 6th 2015

f77 = gfortran
f90 = gfortran
cc = g++

#opt = -O3 -Jmod -pg -mcmodel=large
opt = -O3 -Jmod 
optc = -O3

lib = -L. -lc++ -lstdc++

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
  luminosity/filter.o \
  luminosity/lum_func.o \
  luminosity/luminosities.o \
  luminosity/luminosity_planck_bandpass.o \
  luminosity/luminosity_synthetic_bandpass.o \
  luminosity/luminosity_synthetic_filters.o \
  luminosity/luminosity_synthetic_filters2.o \
  misc/arcsec_au.o \
  misc/au_arcsec.o \
  misc/au_day.o \
  misc/auday_kms.o \
  misc/bessj1.o \
  misc/bessj32.o \
  misc/dotprod.o \
  misc/eps_earth.o \
  misc/extrap.o \
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
  chi2/chi2_func_CLO.o \
  chi2/chi2_func_ECL.o \
  chi2/chi2_func_LC.o \
  chi2/chi2_func_RV.o \
  chi2/chi2_func_SED.o \
  chi2/chi2_func_SED2.o \
  chi2/chi2_func_SKY.o \
  chi2/chi2_func_SKY2.o \
  chi2/chi2_func_SKY3.o \
  chi2/chi2_func_SYN.o \
  chi2/chi2_func_T3.o \
  chi2/chi2_func_TTV.o \
  chi2/chi2_func_VIS.o \
  chi2/distance_AB_C.o \
  chi2/constraint.o \
  chi2/read_CLO.o \
  chi2/read_ECL.o \
  chi2/read_LC.o \
  chi2/read_RV.o \
  chi2/read_SED.o \
  chi2/read_SED2.o \
  chi2/read_SKY.o \
  chi2/read_SKY2.o \
  chi2/read_SYN.o \
  chi2/read_TTV.o \
  chi2/read_VIS.o \
  chi2/read_filter.o \
  chi2/read_synth.o \
  chi2/read_time.o \
  chi2/read_time_all.o \
  chi2/swift_bs_xyzb.o \
  chi2/write_uvw.o \
  geometry/geometries.o \
  geometry/geometry_1centric.o \
  geometry/geometry_3plus2.o \
  geometry/geometry_ecliptic.o \
  geometry/geometry_equatorial.o \
  geometry/geometry_hierarch.o \
  geometry/geometry_hierarch2.o \
  geometry/geometry_twopairs.o \
  geometry/geometry_twopairs2.o \
  geometry/geometry_periods.o \
  simplex/amebsa.o \
  simplex/amoeba.o \
  simplex/amotry.o \
  simplex/amotsa.o \
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
  xv2el/nula2pi.o \
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
  misc/const.o \
  ao/read_pnm.o \
  ao/write_pnm.o \
  ao/write_silh.o \
  ao/shadowing.o \
  ao/silhouette.o \
  main/read_dependent.o \
  misc/nrtype.o \
  misc/nrutil.o \
  misc/srtidx.o \
  misc/srtint.o \
  misc/uvw_nodes.o \
  shape/read_face.o \
  shape/read_node.o \
  shape/read_elem.o \
  shape/write_face.o \
  shape/write_node.o \
  shape/write_edge.o \
  shape/write_elem.o \
  shape/edge.o \
  shape/subdivide.o \
  shape/subdivide_n.o \
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
  multipole/jacobi.o \
  multipole/normalize.o \
  multipole/normal.o \
  multipole/write_multipole.o \
  multipole/getacc_bf.o \
  multipole/getacc_bf2.o \
  multipole/getacc_mp.o \
  multipole/getacc_mp2.o \
  roche/root.o \
  roche/romberg.o \
  roche/roche.o \
  roche/omega_roche_approx.o \
  luminosity/hec88.o \
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
  lc_polygon/revert.o \
  lc_polygon/clip.o \
  lc_polygon/clip2.o \
  lc_polygon/hapke.o \
  lc_polygon/lambert.o \
  lc_polygon/lommel.o \
  lc_polygon/geometric.o \
  lc_polygon/planck.o \
  lc_polygon/read_input.o \
  lc_polygon/surface.o \
  lc_polygon/to_poly.o \
  lc_polygon/to_three.o \
  lc_polygon/uvw.o \
  lc_polygon/write1.o \
  lc_polygon/write_poly.o \
  lc_polygon/xyz.o \
  lc_polygon/centre_of_p.o \
  lc_polygon/normal_of_p.o \
  lc_polygon/shadowing_of_p.o \
  lc_polygon/rotate_of_p.o \
  lc_polygon/lc_polygon1.o \
  adam/center_pnm.o \
  adam/intersect_AB_l.o \
  adam/inside_polygon.o \
  adam/raytrace.o \
  adam/cliptrace.o \
  psf/psf.o \
  psf/wrap.o \
  psf/fourrow.o \
  psf/four2.o \
  psf/realft2.o \
  psf/convolve.o \
  psf/convolve_fft.o \
  chi2/lite.o \
  chi2/read_ephemeris.o \
  chi2/read_AO.o \
  chi2/read_LC2.o \
  chi2/read_OCC.o \
  chi2/write_poles.o \
  chi2/chi2_func_AO.o \
  chi2/chi2_func_AO2.o \
  chi2/chi2_func_OCC.o \
  chi2/chi2_func_LC2.o \
  chi2/chi2_func.o \

objc = \
  lc_polygon/clip_in_c.o \
  adam/crop_in_c.o \
  clipper2/clipper.engine.o \

inc = \
  swift.inc \
  version.inc \
  chi2/chi2.inc \
  chi2/dependent.inc \
  chi2/cb_limb.inc \
  chi2/cb_t3amp.inc \
  filters/filters.inc \
  limcof/limcof.inc \
  luminosity/cb_absol.inc \
  misc/const.inc \
  subplex/usubc.inc \
  tides/spin.inc \
  tides/tides.inc \
  tides2/tides2.inc \
  wd/lc.inc \

all: main/chi2 main/simplex main/simann main/subplex main/swift_bs main/map

main/chi2: main/chi2.f90 $(obj90) $(obj) $(objc) $(inc)
	$(f77) $(opt) $(obj) $(obj90) $(objc) -o $@ $< $(lib)

main/simplex: main/simplex.f90 $(obj90) $(obj) $(objc) $(inc)
	$(f77) $(opt) $(obj) $(obj90) $(objc) -o $@ $< $(lib)

main/simann: main/simann.f90 $(obj90) $(obj) $(objc) $(inc)
	$(f77) $(opt) $(obj) $(obj90) $(objc) -o $@ $< $(lib)

main/subplex: main/subplex.f90 $(obj90) $(obj) $(objc) $(inc)
	$(f77) $(opt) $(obj) $(obj90) $(objc) -o $@ $< $(lib)

main/swift_bs: main/swift_bs.f $(obj90) $(obj) $(objc) $(inc)
	$(f77) $(opt) $(obj) $(obj90) $(objc) -o $@ $< $(lib)

main/map: main/map.f90 $(obj90) $(obj) $(objc) $(inc)
	$(f77) $(opt) $(obj) $(obj90) $(objc) -o $@ $< $(lib)

$(obj90) : %.o:%.f90 $(inc)
	$(f90) $(opt) -c -o $@ $<

$(obj) : %.o:%.f $(inc)
	$(f77) $(opt) -c -o $@ $<

$(objc) : %.o:%.cpp $(inc)
	$(cc) $(optc) -std=c++17 -c -o $@ $<

clean : FORCE
	rm -f mod/*.mod
	rm -f $(obj) $(obj90) $(objc)

FORCE :


