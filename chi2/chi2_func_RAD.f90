! chi2_func_RAD.f90
! Calculate chi^2 for adaptive-optics data; ADAM version.
! Miroslav Broz (miroslav.broz@email.cz), Aug 27th 2022

module chi2_func_RAD_module

contains

subroutine chi2_func_RAD(NOUT, tout, rl, vl, chi2, n)

use const_module
use read_rad_module
use read_pnm_module
use read_ephemeris_module
use write_pnm_module
use center_pnm_module
use rotate_module
use lc_polygon_module
use raytrace_module
use cliptrace_module
!use psf_module
!use wrap_module
!use convolve_fft_module
use doppler_module
use surface2_module

implicit none
include '../filters/filters.inc'
include 'chi2.inc'
include 'dependent.inc'

integer, intent(in) :: NOUT
double precision, dimension(OUTMAX), intent(in) :: tout
double precision, dimension(OUTMAX,NBODMAX,3), intent(in) :: rl, vl
double precision, intent(out) :: chi2
integer, intent(out) :: n

! observational
integer, save :: m_OBS = 0
integer, dimension(RADMAX), save :: dataset
double precision, dimension(RADMAX), save :: t_OBS, freq, pixel_scale1, pixel_scale2, vardist, ecl, ecb
character(len=255), dimension(RADMAX), save :: file_OBS
integer, save :: N_s
double precision, dimension(OUTMAX), save :: t_s, vardist_s, ecl_s, ecb_s

! temporary
integer :: i, j, j1, j2, k, m, w, h
integer :: i2nd
integer :: iband
double precision, dimension(:,:), pointer :: pnm, pnm_OBS, pnm_res, pnm_psf
double precision, dimension(:,:), pointer, save :: psf, psf_
double precision :: t_interp, lite, l, b
double precision :: mag, scl, sigma2, chi_, chi
double precision :: d_ts, d_to
double precision, dimension(3) :: n_ts, n_to
double precision :: xl_interp, yl_interp, zl_interp
double precision :: vxl_interp, vyl_interp, vzl_interp
double precision, dimension(NBODMAX,3) :: r_interp, v_interp
double precision, dimension(3) :: hatu, hatv, hatw
double precision :: eps, zeta
double precision, dimension(2) :: c, c_
double precision :: tmp
double precision :: capS
character(len=80) :: str, str_
integer, parameter :: iu = 15
integer, save :: i1st = 0

! functions
double precision, external :: interp, interp2, eps_earth

if (.not.use_adam) return

!
! read observational data
!
if (i1st.eq.0) then

  call read_RAD(file_RAD, m_OBS, t_OBS, freq, pixel_scale1, pixel_scale2, vardist, ecl, ecb, dataset, file_OBS)

  if (debug) then
    write(*,*) "# m_RAD = ", m_OBS
  endif

! read Sun ephemeris
!  if (m_OBS.gt.0) then
!    call read_ephemeris("ephemeris_S.dat", N_s, t_s, vardist_s, ecl_s, ecb_s)
!  endif

!  if (use_stellar) then
!    call read_pnm("stellar.pnm", psf)
!    psf = psf/sum(psf)
!  endif

  i1st = 1
endif  ! i1st

!
! calculate the chi^2 value (adaptive-optics data)
!

chi2 = 0.d0
n = 0

if (debug) then
  open(unit=iu, file="chi2_RAD.dat", status="unknown")
  write(iu,*) "# i & j & k & pnm(j,k) & pnm_OBS(j,k) & sigma [adu] & chi^2"
endif

j1 = 2
j2 = 2
i2nd = 0
iband = 7

do i = 1, m_OBS

! target-observer
  l = ecl(i)
  b = ecb(i)
  d_to = vardist(i)

  n_to = -(/cos(l)*cos(b), sin(l)*cos(b), sin(b)/)

! light-time effect
  if (use_vardist) then
    lite = -d_to/clight * AU/day 
  else
    lite = 0.d0
  endif
  t_interp = t_OBS(i) + lite

! target-sun
!  do while ((j2.lt.N_s).and.(t_s(j2).le.t_interp))
!    j2 = j2+1
!  enddo

!  l = interp2(t_s(j2-1), t_s(j2), ecl_s(j2-1), ecl_s(j2), t_interp)
!  b = interp(t_s(j2-1), t_s(j2), ecb_s(j2-1), ecb_s(j2), t_interp)
!  d_ts = interp(t_s(j2-1), t_s(j2), vardist_s(j2-1), vardist_s(j2), t_interp)

!  n_ts = -(/cos(l)*cos(b), sin(l)*cos(b), sin(b)/)

! Note: assuming a monostatic radar (trasmitter == receiver)
   d_ts = d_to
   n_ts = n_to

! orbiting bodies
  do while ((j1.lt.NOUT).and.(tout(j1).le.t_interp))
    j1 = j1+1
  enddo

  do j = 1, nbod
    xl_interp = interp(tout(j1-1), tout(j1), rl(j1-1,j,1), rl(j1,j,1), t_interp)  ! l-th body, x coordinate
    yl_interp = interp(tout(j1-1), tout(j1), rl(j1-1,j,2), rl(j1,j,2), t_interp)  ! y coordinate
    zl_interp = interp(tout(j1-1), tout(j1), rl(j1-1,j,3), rl(j1,j,3), t_interp)  ! z coordinate

    r_interp(j,:) = (/xl_interp, yl_interp, zl_interp/)

    vxl_interp = interp(tout(j1-1), tout(j1), vl(j1-1,j,1), vl(j1,j,1), t_interp)  ! l-th body, x coordinate
    vyl_interp = interp(tout(j1-1), tout(j1), vl(j1-1,j,2), vl(j1,j,2), t_interp)  ! y coordinate
    vzl_interp = interp(tout(j1-1), tout(j1), vl(j1-1,j,3), vl(j1,j,3), t_interp)  ! z coordinate

    v_interp(j,:) = (/vxl_interp, vyl_interp, vzl_interp/)
  enddo  ! j

!
! observed delay-doppler image
!

  call read_pnm(file_OBS(i), pnm_OBS)

  w = size(pnm_OBS,1)
  h = size(pnm_OBS,2)

! centering
!  c = center_pnm(pnm_OBS, pixel_scale1(i))
  c = 0.d0  ! dbg

!
! synthetic image; computed with lc_polygon
!

! Note: polys5, Phi_e, normals, photocentre ... lc_polygon module variables

  call lc_polygon(t_interp, lite, r_interp*au, n_ts, n_to, d_ts*au, d_to*au, &
    lambda_eff(iband), band_eff(iband), calib(iband), mag, i2nd)

! centering
!  c_ = photocentre(1:2)/(d_to*au)
  c_ = 0.d0  ! dbg

  if (debug) then
    write(*,*) '# c  = ', c/(pixel_scale1(i)*arcsec), ' pxl'
    write(*,*) '# c_ = ', c_/(pixel_scale1(i)*arcsec), ' pxl'
  endif

! Doppler-delay
  call doppler(polys5, polys6, freq(i), r_interp*au, v_interp*au/day)

  capS = surface2(polys6, polys5, normals, surf6)

! Note: Phi_e must be further multiplied by the surface ratios
! in the u, v, w space / in the doppler-delay space, because
! the coordinates are different (m's versus Hz, mus), but
! the amount of radio signal reflected by a normal area is
! "compressed and conserved" to the corresponding doppler-delay area.

  Phi_e = Phi_e * surf/surf6

! Note: For radar, d_to = 1.0, since it's distance independent.

! raytracing
  if (use_cliptrace) then
    call cliptrace(polys6, Phi_e, mu_e, normals, 1.0d0, pixel_scale1(i)/arcsec, -c + c_, w, h, pnm, pixel_scale2=pixel_scale2(i)/arcsec)
  else
    call raytrace(polys6, Phi_e, mu_e, 1.0d0, pixel_scale1(i), -c + c_, w, h, pnm)
  endif

! psf
!  if (.not.use_stellar) then
!    allocate(psf(w, h))
!    call psf_moffat(psf_param(1), psf_param(2), psf)
!  endif

!  allocate(psf_(w, h))
!  call wrap(psf, psf_)
!  allocate(pnm_psf(w, h))
!  call convolve_fft(pnm, psf_, pnm_psf)

! scaling
!  scl = maxval(pnm_OBS)/maxval(pnm_psf)
!  pnm_psf = pnm_psf*scl
  scl = maxval(pnm_OBS)/maxval(pnm)
  pnm = pnm*scl

  if (debug) then
    write(*,*) '# scl = ', scl
  endif

! chi^2
  allocate(pnm_res(w, h))
  pnm_res = 0.d0

!  tmp = silh_factor*maxval(pnm_OBS)
  tmp = 0.0d0
  chi = 0.d0
  m = 0

  if (debug) then
    write(iu,*) '# t_OBS = ', t_OBS(i)
    write(iu,*) '# t_interp = ', t_interp
    write(iu,*) '# lite = ', lite
    write(iu,*) '# dataset = ', dataset(i)
    write(iu,*) '# file_OBS = ', trim(file_OBS(i))
  endif

  do j = 1, w
    do k = 1, h

      if ((pnm(j,k).gt.tmp).or.(pnm_OBS(j,k).gt.tmp)) then

        sigma2 = max(pnm(j,k),pnm_OBS(j,k))
        chi_ = (pnm(j,k) - pnm_OBS(j,k))**2/sigma2
        chi = chi  + chi_
        chi2 = chi2 + chi_
        pnm_res(j,k) = chi_
        n = n+1
        m = m+1

        if (debug) then
          write(iu,*) i, j, k, pnm(j,k), pnm_OBS(j,k), sqrt(sigma2), chi_
        endif

      else
        pnm_res(j,k) = 0.d0
      endif

    enddo  ! k
  enddo  ! j

  if (debug_swift) then
    write(iu,*)

    write(str_,'(i4.4)') i
    str = 'doppler.' // trim(str_) // '.syn.pnm'
    call write_pnm(str, pnm)

    str = 'doppler.' // trim(str_) // '.obs.pnm'
    call write_pnm(str, pnm_OBS)

    str = 'doppler.' // trim(str_) // '.res.pnm'
    call write_pnm(str, pnm_res)

!    str = 'output.' // trim(str_) // '.psf.pnm'
!    call write_pnm(str, pnm_psf)

!    psf_ = psf/maxval(psf)*65535.d0
!    call write_pnm("psf.pnm", psf_)
  endif

  deallocate(pnm)
  deallocate(pnm_OBS)
  deallocate(pnm_res)
!  deallocate(pnm_psf)
!  if (.not.use_stellar) deallocate(psf)
!  deallocate(psf_)

enddo  ! i

if (debug) then
  close(iu)
endif

return
end subroutine chi2_func_RAD

end module chi2_func_RAD_module


