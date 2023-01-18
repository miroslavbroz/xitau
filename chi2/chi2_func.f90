! chi2_func.f90
! Calculate chi^2 for a given state vector x().
! Miroslav Broz (miroslav.broz@email.cz), Feb 18th 2022

! Notation:
!
! m             .. mass [M_S]
! r             .. initial coordinates [au]
! rb            .. barycentric coordinates [au]
! rh            .. heliocentric coordinates [au]
! rp            .. photocentric coordinates [au]
! rp3           .. (1+2+3)-photocentric coordinates [au]
! v             .. ditto velocities [au d^-1]
! vb            .. ditto velocities [au d^-1]
! vh            .. ditto velocities [au d^-1]
! vp            .. ditto velocities [au d^-1]
! vp3           .. ditto velocities [au d^-1]
! q             .. mass ratio [1]
! elmts         .. orbital elements, Keplerian [au, 1, 1, deg, deg, deg]
! tout          .. time [JD(TDB)]
! tout1         .. time [d]
! tout2         .. time [d]
! T0            .. epoch [JD(TDB)]
! R_star        .. stellar radius [R_S]
! T_eff         .. effective temperature [K]
! Lum           .. passband luminosity [W]
! log_g         .. logarithm of gravitational acceleration [cm s^-2]
! v_rot         .. rotational velocity [km s^-1]
! P_rot         .. rotational period [d]
! metal         .. metallicity [solar]
! Delta_t       .. tidal time lag [s]
! C20           .. quadrupole moment, J2 = -C20 [1]
! pole_l        .. pole ecliptic longitude [deg]
! pole_b        .. pole ecliptic latitude [deg]
! phi0          .. rotation phase [deg]
! albedo        .. geometric albedo [1]
! scattering(1) .. B0, ampltiude of opposition effect [1]
! scattering(2) .. minh, width of o. e. [rad]
! scattering(3) .. ming, asymmetry factor [1]
! scattering(4) .. bartheta, mean slope [deg]
! psf_param(1)  .. alpha, width of the Moffat function [pxl]
! psf_param(2)  .. beta, exponent of M. f. [1]
! zero          .. magnitude zero point [mag]
! gamma         .. systemic velocity [km s^-1]
! d_pc          .. distance [pc]
! omega         .. rotation frequency [rad s^-1]
! s             .. spin vector [1]

module chi2_func_module

contains

double precision function chi2_func(x)

use const_module, pi_=>pi
use read_face_module
use read_node_module
use chi2_func_AO_module
use chi2_func_AO2_module
use chi2_func_LC2_module
use chi2_func_OCC_module

include '../swift.inc'
include '../tides/spin.inc'
include '../tides/tides.inc'
include 'chi2.inc'
include 'dependent.inc'

double precision, dimension(ndim) :: x

! variables for numerical integration
integer, save :: nout1, nout2, nout
double precision, dimension(NBODMAX) :: m
double precision, dimension(NBODMAX,3) :: r, v
double precision, dimension(NBODMAX,6) :: elmts
double precision, dimension(OUTMAX) :: tout
double precision, dimension(OUTMAX,NBODMAX,3) :: rb, vb, rh, vh, rp, vp, rp3, vp3
double precision, dimension(OUTMAX1) :: tout1
double precision, dimension(OUTMAX2) :: tout2
double precision, dimension(OUTMAX1,NBODMAX,3) :: rout1, vout1
double precision, dimension(OUTMAX2,NBODMAX,3) :: rout2, vout2
double precision, dimension(NBODMAX), save :: omega0
double precision, dimension(NBODMAX,3), save :: s0
character(len=80) :: inparfile

! internal variables
integer :: i, j, k, l, ierr, iu=10
integer :: ialpha, nmin
double precision :: chi2
double precision, dimension(3) :: r_photocentre, v_photocentre
double precision :: Lumtot
double precision :: lambda_eff, band_eff
double precision, dimension(MINMAX) :: tmin, duration
double precision :: gamma, gamma_auday
integer :: n_of_interest
double precision, dimension(TIMEMAX) :: t_of_interest, t_of_interest1, t_of_interest2
double precision :: msum
double precision, dimension(NBODMAX) :: q
double precision :: dummy
integer, save :: i1st=0

! functions
double precision, external :: merit_func, kms_auday

! get both free and fixed parameters

j = 0
do i = 1, nparam
  if (variable(i)) then
    j = j+1
    x_param(i) = x(j)
  endif
enddo

write(*,'(a,$)') "# x() array:"
write(*,*) (x(i), i=1,ndim)

! parameters -> variables, arrays for easy manipulation

j = 0
!do i = 1, nbod
!  j = j+1
!  m(i) = x_param(j)*GM_S
!enddo
j = j+1
msum = x_param(j)*GM_S
do i = 2, nbod
  j = j+1
  q(i) = x_param(j)
enddo

do i = 2, nbod
  do k = 1, 6
    j = j+1
    elmts(i,k) = x_param(j)
  enddo
enddo

do i = 1, nbod
  j = j+1
  T_eff(i) = x_param(j)
enddo
do i = 1, nbod
  j = j+1
  R_star(i) = x_param(j)
enddo
!do i = 1, nbod
!  j = j+1
!  log_g(i) = x_param(j)
!enddo
!do i = 1, nbod
!  j = j+1
!  v_rot(i) = x_param(j)
!enddo
do i = 1, nbod
  j = j+1
  P_rot(i) = x_param(j)
enddo
do i = 1, nbod
  j = j+1
  metal(i) = x_param(j)
enddo
do i = 1, nbod
  j = j+1
  Delta_t(i) = x_param(j)/day
enddo
do i = 1, nbod
  j = j+1
  C20(i) = x_param(j)
enddo
do i = 1, nbod
  j = j+1
  pole_l(i) = x_param(j)*deg
enddo
do i = 1, nbod
  j = j+1
  pole_b(i) = x_param(j)*deg
enddo
do i = 1, nbod
  j = j+1
  phi0(i) = x_param(j)*deg
enddo
do i = 1, nbod
  j = j+1
  albedo(i) = x_param(j)
enddo

do i = 1, 4
  j = j+1
  scattering(i) = x_param(j)
enddo
scattering(4) = scattering(4)*deg  ! bartheta

do i = 1, 2
  j = j+1
  psf_param(i) = x_param(j)
enddo

j = j+1
gamma = x_param(j)
j = j+1
d_pc = x_param(j)

if (j.ne.nparam) then
  write(*,*) "chi2_func.f: Error number of parameters is ", j, ".ne.nparam = ", nparam
  stop
endif

! convert ratios to masses
m(1) = msum
do i = 2, nbod
  m(1) = m(1)/(1.d0+q(i))
  m(i) = 0.d0
enddo
do i = 2, nbod
  do j = 1, i-1
    m(i) = m(i)+m(j)
  enddo
  m(i) = q(i)*m(i)
enddo

if (debug) then
  do i = 1, nbod
    write(*,*) '# m(', i, ') = ', m(i)/GM_S, ' M_S'
  enddo
endif

! constrain orbital inclinations and nodes by pole (equator) of 1

if (use_varpole) then
  do i = 2, nbod
    elmts(i,3) = 90.d0-pole_b(1)/deg
    elmts(i,4) = 180.d0+pole_l(1)/deg
  enddo
endif

! EITHER, compute log g [cgs] from m, R_star

do i = 1, nbod
  log_g(i) = log10(m(i)*AU**3/day**2/(R_star(i)*R_S)**2*100.d0)
enddo

if (debug_swift) then
  do i = 1, nbod
    write(*,*) '# log_g(', i, ') = ', log_g(i)
  enddo
endif

! OR, compute R_star [R_S] from m, log_g

!do i = 1, nbod
!  R_star(i) = sqrt(m(i)*AU**3/day**2/(10.d0**log_g(i)/100.d0))/R_S
!enddo
!
!if (debug_swift) then
!  do i = 1, nbod
!    write(*,*) '# R_star(', i, ') = ', R_star(i), ' R_S'
!  enddo
!endif

! EITHER, compute P_rot [d] from R_star, v_rot
 
!do i = 1, nbod
!  P_rot(i) = R_star(i)*R_S/(v_rot(i)*1.d3)/day
!enddo
!
!if (debug_swift) then
!  do i = 1, nbod
!    write(*,*) '# P_rot(', i, ') = ', P_rot(i), ' d'
!  enddo
!endif
 
! OR, compute v_rot [km/s] from R_star, P_rot

do i = 1, nbod
  v_rot(i) = R_star(i)*R_S/(P_rot(i)*day)/1.d3
enddo

if (debug_swift) then
  do i = 1, nbod
    write(*,*) '# v_rot(', i, ') = ', v_rot(i), ' km/s'
  enddo
endif

! constrain some of the components by Harmanec (1988) relations

do i = 1, nbod
  if (use_hec88(i)) then
    call hec88(T_eff(i), R_star(i), m(i), dummy, log_g(i))

    m(i) = m(i)*GM_S

    if (debug) then
      write(*,*) '# Using Harmanec (1988) for component no. ', i
      write(*,*) '# T_eff(', i, ') = ', T_eff(i), ' K'
      write(*,*) '# R_star(', i, ') = ', R_star(i), ' R_S'
      write(*,*) '# m(', i, ') = ', m(i)/GM_S, ' M_S'
      write(*,*) '# log_g(', i, ') = ', log_g(i), ' [cgs]'
    endif
  endif
enddo

! compute luminosities (for photocentre computations)

lambda_eff = 545.d-9  ! m
band_eff = 85.d-9  ! m

call luminosities(T_eff, R_star, nbod, lambda_eff, band_eff, Lum, Lumtot, use_planck)

if (debug_swift) then
  do i = 1, nbod
    write(*,*) '# Lum(', i, ') = ', Lum(i)/Lumtot
  enddo
endif

! convert elements to coordinates (wrt. the geometry of the system)

call geometries(nbod, m, elmts, r, v, geometry)

if (debug_swift) then
  write(*,*) "# barycentric coordinates:"
  do i = 1, nbod
    write(*,*) (r(i,k), k=1,3), (v(i,k), k=1,3)
  enddo

  write(*,*) "# 1-centric coordinates:"
  do i = 1, nbod
    write(*,*) (r(i,k)-r(1,k), k=1,3), (v(i,k)-v(1,k), k=1,3)
  enddo
endif

!-----------------------------------------------------------------------

if (i1st.eq.0) then

! read times of interest
  call read_time_all(n_of_interest, t_of_interest)

  do i = 1, n_of_interest
    t_of_interest1(i) = t_of_interest(i)-T0
    j = n_of_interest-i+1
    t_of_interest2(j) = T0-t_of_interest(i)
  enddo

! read shape
  call read_node("input.node", nodesforchi)
  call read_face("input.face", facesforchi)

! init tidal code
  call io_init_spin("spin.in", nbod)
  call io_init_tides("tides.in", nbod)
  call io_init_tides2("tides2.in", nbod)

! save values from spin.in
  do i = 1, nbod
    omega0(i) = omega(i)
  enddo

  i1st = 1
endif

! modify values from spin.in, tides.in
do i = 1, nbod
  s0(i,1) = cos(pole_l(i))*cos(pole_b(i))
  s0(i,2) = sin(pole_l(i))*cos(pole_b(i))
  s0(i,3) = sin(pole_b(i))
enddo

do i = 1, nbod
  Delta_t_(i) = Delta_t(i)
enddo

!-----------------------------------------------------------------------

! forward integration of orbits
 
inparfile = "param.in"
is_forward = .true.

call swift_bs_xyzb(nbod,m,r,v,omega0,s0,nout1,OUTMAX1,tout1,rout1,vout1,inparfile,eps_BS,debug,n_of_interest,t_of_interest1)

! backward integration (i.e., with reversed velocities)

do i = 1, nbod
  do j = 1, 3
    v(i,j) = -v(i,j)
    s0(i,j) = -s0(i,j)
  enddo
enddo

inparfile = "param_BACK.in"
is_forward = .false.

call swift_bs_xyzb(nbod,m,r,v,omega0,s0,nout2,OUTMAX2,tout2,rout2,vout2,inparfile,eps_BS,debug,n_of_interest,t_of_interest2)

! merge the integration output and add Julian date

nout = nout1+nout2
if (nout.gt.OUTMAX) then
  write(*,*) "chi2_func: Error number of output data > OUTMAX = ", OUTMAX
  stop
endif

if (debug) then
  write(*,*) "# nout = ", nout
endif

do i = 1, nout2
  l = nout2-i+1
  tout(l) = T0 - tout2(i)
  do j = 1, nbod
    do k = 1,3
      rb(l,j,k) = rout2(i,j,k)
      vb(l,j,k) = -vout2(i,j,k)
    enddo
  enddo
enddo

do i = 1, nout1
  l = nout2+i
  tout(l) = T0 + tout1(i)
  do j = 1, nbod
    do k = 1,3
      rb(l,j,k) = rout1(i,j,k)
      vb(l,j,k) = vout1(i,j,k)
    enddo
  enddo
enddo

! add systemic velocity

gamma_auday = kms_auday(gamma)
do i = 1, nout
  do j = 1, nbod
    vb(i,j,3) = vb(i,j,3) + gamma_auday
  enddo
enddo

if (debug_swift) then
  open(unit=iu,file="out_JDATE_barycentric.dat",status="unknown")
  do i = 1, nout
    do j = 1, nbod
      write(iu,*) tout(i),-j,rb(i,j,1),rb(i,j,2),rb(i,j,3),vb(i,j,1),vb(i,j,2),vb(i,j,3)
    enddo
  enddo
  close(iu)
endif

! convert to heliocentric (1-centric) coordinates

do i = 1, nout
  do j = 1, nbod
    do k = 1, 3
      rh(i,j,k) = rb(i,j,k) - rb(i,1,k)
      vh(i,j,k) = vb(i,j,k) - vb(i,1,k)
    enddo
  enddo
enddo

if (debug_swift) then
  open(unit=iu,file="out_JDATE_heliocentric.dat",status="unknown")
  do i = 1, nout
    do j = 1, nbod
      write(iu,*) tout(i),-j,rh(i,j,1),rh(i,j,2),rh(i,j,3),vh(i,j,1),vh(i,j,2),vh(i,j,3)
    enddo
  enddo
  close(iu)
endif

! convert to photocentric (1+2) coordinates

do i = 1, nout
  do k = 1, 3
    Lumtot = 0.d0
    r_photocentre(k) = 0.d0
    v_photocentre(k) = 0.d0
    do j = 1, min(2, nbod)
       Lumtot = Lumtot + Lum(j)
       r_photocentre(k) = r_photocentre(k) + rb(i,j,k)*Lum(j)
       v_photocentre(k) = v_photocentre(k) + vb(i,j,k)*Lum(j)
    enddo
    r_photocentre(k) = r_photocentre(k)/Lumtot
    v_photocentre(k) = v_photocentre(k)/Lumtot
  enddo
  do j = 1, nbod
    do k = 1, 3
      rp(i,j,k) = rb(i,j,k) - r_photocentre(k)
      vp(i,j,k) = vb(i,j,k) - v_photocentre(k)
    enddo
  enddo
enddo

if (debug_swift) then
  open(unit=iu,file="out_JDATE_photocentric.dat",status="unknown")
  do i = 1, nout
    do j = 1, nbod
      write(iu,*) tout(i),-j,rp(i,j,1),rp(i,j,2),rp(i,j,3),vp(i,j,1),vp(i,j,2),vp(i,j,3)
    enddo
  enddo
  close(iu)
endif

! convert to another photocentric (1+2+3) coordinates

do i = 1, nout
  do k = 1, 3
    Lumtot = 0.d0
    r_photocentre(k) = 0.d0
    v_photocentre(k) = 0.d0
    do j = 1, min(3, nbod)
       Lumtot = Lumtot + Lum(j)
       r_photocentre(k) = r_photocentre(k) + rb(i,j,k)*Lum(j)
       v_photocentre(k) = v_photocentre(k) + vb(i,j,k)*Lum(j)
    enddo
    r_photocentre(k) = r_photocentre(k)/Lumtot
    v_photocentre(k) = v_photocentre(k)/Lumtot
  enddo
  do j = 1, nbod
    do k = 1, 3
      rp3(i,j,k) = rb(i,j,k) - r_photocentre(k)
      vp3(i,j,k) = vb(i,j,k) - v_photocentre(k)
    enddo
  enddo
enddo

!if (debug_swift) then
!  open(unit=iu,file="out_JDATE_photocentric3.dat", status="unknown")
!  do i = 1, nout
!    do j = 1, nbod
!      write(iu,*) tout(i),-j,rp3(i,j,1),rp3(i,j,2),rp3(i,j,3),vp3(i,j,1),vp3(i,j,2),vp3(i,j,3)
!    enddo
!  enddo
!  close(iu)
!endif

! optionally, write (u, v, w) coordinates

if (debug_swift.and.use_vardist) then
  call write_uvw(nout, tout, rh, rp, rp3)
endif

lns = 0.d0  ! sum of ln sigma_i (for MCMC)

!
! calculate the chi^2 values
!

!   1. speckle-interferometry data
!   2. radial-velocity data
!   3. transit-timing variations
!   4. eclipse durations
!   5. interferometric visibilities
!   6. interferometric closure phases
!   7. interferometric triple product amplitude
!   8. light curve (u. Wilson-Devinney)
!   9. synthetic spectra
!  10. spectral-energy distribution
!  11. adaptive-optics imaging (u. silhouettes)
!  12. adaptive-optics imaging (u. lc_polygon algorithm)
!  13. differential astrometry
!  14. angular velocity
!  15. occultation timings
!  16. light curve (u. lc_polygon algorithm)

call chi2_func_SKY(nout, tout, rh, rp, rp3, chi2_SKY, n_SKY)

call chi2_func_RV(nout, tout, vb, chi2_RV, n_RV)

call chi2_func_TTV(nout, nout2, m, tout, rh, rb, nmin, tmin, duration, chi2_TTV, n_TTV)

call chi2_func_ECL(nmin, tmin, duration, chi2_ECL, n_ECL)

call chi2_func_VIS(nout, m, tout, rh, chi2_VIS, n_VIS)

call chi2_func_CLO(nout, tout, rh, chi2_CLO, n_CLO)

call chi2_func_T3(chi2_T3, n_T3)

call chi2_func_LC(nout, nout2, m, tout, rh, vh, rb, chi2_LC, n_LC)

call chi2_func_SYN(nout, tout, vb, chi2_SYN, n_SYN)

call chi2_func_SED(chi2_SED, n_SED)

call chi2_func_AO(chi2_AO, n_AO)

call chi2_func_AO2(nout, tout, rh, vh, chi2_AO2, n_AO2)

call chi2_func_SKY2(nout, tout, rh, chi2_SKY2, n_SKY2)

call chi2_func_SKY3(nout, tout, rh, vh, chi2_SKY3, n_SKY3)

call chi2_func_OCC(nout, tout, rh, chi2_OCC, n_OCC)

call chi2_func_LC2(nout, tout, rh, vh, chi2_LC, n_LC)

! add artificial term(s) to constrain parameters

chi2_MASS = 0.d0
do i = 1, nbod
  chi2_MASS = chi2_MASS + merit_func(m(i)/GM_S,m_min(i),m_max(i))
enddo

! sum everything

n_fit = n_SKY + n_RV + n_TTV + n_ECL + n_VIS + n_CLO + n_T3 + n_LC &
  + n_SYN + n_SED + n_AO + n_AO2 + n_SKY2 + n_SKY3 + n_OCC

chi2 = w_SKY*chi2_SKY + w_RV*chi2_RV + w_TTV*chi2_TTV + w_ECL*chi2_ECL &
  + w_VIS*chi2_VIS + w_CLO*chi2_CLO + w_T3*chi2_T3 + w_LC*chi2_LC &
  + w_SYN*chi2_SYN + w_SED*chi2_SED + w_AO*chi2_AO + w_AO2*chi2_AO2 &
  + w_SKY2*chi2_SKY2 + w_SKY3*chi2_SKY3 + w_OCC*chi2_OCC + chi2_MASS

write(*,'(a,$)') "# n values: "
write(*,*) n_SKY, n_RV, n_TTV, n_ECL, n_VIS, n_CLO, n_T3, n_LC, &
  n_SYN, n_SED, n_AO, n_AO2, n_SKY2, n_SKY3, n_OCC, n_fit

write(*,'(a,$)') "# chi^2 values: "
write(*,*) chi2_SKY, chi2_RV, chi2_TTV, chi2_ECL, chi2_VIS, chi2_CLO, &
  chi2_T3, chi2_LC, chi2_SYN, chi2_SED, chi2_AO, chi2_AO2, chi2_SKY2, &
  chi2_SKY3, chi2_OCC, chi2_MASS, chi2

! write hi-precision output

open(unit=iu, file="chi2_func.tmp", access="append")
write(iu,*) (x_param(j), j = 1,nparam), &
  n_SKY, n_RV, n_TTV, n_ECL, n_VIS, n_CLO, n_T3, n_LC, n_SYN, n_SED, &
  n_AO, n_AO2, n_SKY2, n_SKY3, n_OCC, n_fit, &
  chi2_SKY, chi2_RV, chi2_TTV, chi2_ECL, chi2_VIS, chi2_CLO, chi2_T3, &
  chi2_LC, chi2_SYN, chi2_SED, chi2_AO, chi2_AO2, chi2_SKY2, chi2_SKY3, &
  chi2_OCC, chi2_MASS, chi2
close(iu)

chi2_func = chi2
return
end function chi2_func

end module chi2_func_module


