! chi2_func_LC2.f90
! Compute lightcurve for a polygonal mesh (incl. chi^2).
! Miroslav Broz (miroslav.broz@email.cz), Nov 8th 2022

module chi2_func_LC2_module

contains

subroutine chi2_func_LC2(NOUT, tout, rh, vh, chi2, n)

use const_module
use read_LC2_module
use lc_polygon1_module

implicit none
include '../filters/filters.inc'
include 'simplex.inc'
include 'dependent.inc'

integer, intent(in) :: NOUT
double precision, dimension(OUTMAX), intent(in) :: tout
double precision, dimension(OUTMAX,NBODMAX,3), intent(in) :: rh, vh
double precision, intent(out) :: chi2
integer, intent(out) :: n

! observational data
integer, dimension(BANDMAX), save :: m_OBS
double precision, dimension(OBSMAX,BANDMAX), save :: t_OBS, mag_OBS, sigma_mag_OBS
double precision, dimension(OBSMAX,BANDMAX), save :: vardist, ecl, ecb
integer, dimension(OBSMAX,BANDMAX), save :: dataset

integer, save :: N_s
double precision, dimension(OUTMAX), save :: t_s, vardist_s, ecl_s, ecb_s

! internal variables
integer :: i, j, k, j1, j2
integer :: iband
integer, dimension(BANDMAX), save :: m_BIN
double precision, dimension(OBSMAX,BANDMAX), save :: t_BIN, mag
integer, dimension(OBSMAX,BANDMAX), save :: i_BIN
double precision, dimension(NBODMAX,3) :: r_interp
double precision, dimension(3) :: n_to, n_ts
double precision :: d_to, d_ts
double precision :: eps, lite, t_interp, mag_interp, l, b, chi2_
double precision :: xh_interp, yh_interp, zh_interp
double precision :: alpha, H0
double precision :: zero2

! functions
double precision, external :: interp, interp2

integer, parameter :: iu = 15
integer, save :: i1st = 0, i3rd = 0

if (.not.use_polygon) return

!
! read lightcurve data (only 1st time!)
!
if (i1st.eq.0) then

  do k = 1, nband

    call read_LC2(file_LC(k), m_OBS(k), t_OBS(1,k), mag_OBS(1,k), sigma_mag_OBS(1,k), &
      vardist(1,k), ecl(1,k), ecb(1,k), dataset(1,k))

    call read_ephemeris("ephemeris_S.dat", N_s, t_s, vardist_s, ecl_s, ecb_s)

    if (debug) then
      write(*,*) "# m_LC(", k, ") = ", m_OBS(k)
    endif
!
! Select kind of important times of observations
! and use binning if the cadence is uselessly high.
! This is only for the computation of synthetic LC;
! the complete observed LC will be used for chi^2.
!
    eps = 1.d-8
    if (m_OBS(k).ge.1) then
      j = 1
      t_BIN(j,k) = t_OBS(1,k)-eps
      i_BIN(j,k) = 1
 
      do i = 2, m_OBS(k)
        if (t_OBS(i,k)-t_BIN(j,k) > lightcurve_timestep) then
          j = j + 1
          t_BIN(j,k) = t_OBS(i,k)
          i_BIN(j,k) = i
        endif
      enddo
 
      i = m_OBS(k)
      j = j + 1
      t_BIN(j,k) = t_OBS(i,k)+eps
      i_BIN(j,k) = i
      m_BIN(k) = j
    else
      m_BIN(k) = 0
    endif

  enddo  ! k

  i1st = 1

endif  ! i1st

if (debug) then
  open(unit=iu, file="lightcurve2.dat", status="unknown")
  write(iu,*) "# JD & magnitude & iband & lite [d] & alpha [deg] & H0 [mag]"
endif

if (debug) then
  do k = 1, nband
    iband = iband_LC(k)
    write(*,*) '# iband = ', iband
    write(*,*) '# lambda_eff = ', lambda_eff(iband), ' m'
    write(*,*) '# band_eff = ', band_eff(iband), ' m'
    write(*,*) '# calib = ', calib(iband), ' J s^-1 m^-2 m^-1'
  enddo
endif
 
!
! interpolate trajectories to the (binned) times of observations
! 

do k = 1, nband

  iband = iband_LC(k)
  j1 = 2
  j2 = 2

  do i = 1, m_BIN(k)

! 2DO missing lite for the Sun geometry! iterations?
! alternatively, use vardist's associated to observations;
! nevertheless, we do include lite for bodies 1, 2, 3!

!
! target-observer
!
    j = i_BIN(i,k)
    l = ecl(j,k)
    b = ecb(j,k)
    d_to = vardist(j,k)

    n_to = -(/cos(l)*cos(b), sin(l)*cos(b), sin(b)/)
!
! target-sun (interpolated)
!
    t_interp = t_BIN(i,k)

    do while ((j2.lt.N_s).and.(t_s(j2).le.t_interp))
      j2 = j2+1
    enddo

    l = interp2(t_s(j2-1), t_s(j2), ecl_s(j2-1), ecl_s(j2), t_interp)
    b = interp(t_s(j2-1), t_s(j2), ecb_s(j2-1), ecb_s(j2), t_interp)
    d_ts = interp(t_s(j2-1), t_s(j2), vardist_s(j2-1), vardist_s(j2), t_interp)

    n_ts = -(/cos(l)*cos(b), sin(l)*cos(b), sin(b)/)
!
! light-time effect
!
    if (use_vardist) then
      lite = -d_to/clight * AU/day 
    else
      lite = 0.d0
    endif
    t_interp = t_BIN(i,k) + lite

    do while ((j1.lt.NOUT).and.(tout(j1).le.t_interp))
      j1 = j1 + 1
    enddo

    do j = 1, nbod
      xh_interp = interp(tout(j1-1), tout(j1), rh(j1-1,j,1), rh(j1,j,1), t_interp)  ! l-th body, x coordinate
      yh_interp = interp(tout(j1-1), tout(j1), rh(j1-1,j,2), rh(j1,j,2), t_interp)  ! y coordinate
      zh_interp = interp(tout(j1-1), tout(j1), rh(j1-1,j,3), rh(j1,j,3), t_interp)  ! z coordinate

      r_interp(j,:) = (/xh_interp, yh_interp, zh_interp/)
    enddo  ! j

!
! compute magnitude with the lc_polygon code (SI units)
!
    call lc_polygon1(t_interp, r_interp*au, n_ts, n_to, d_ts*au, d_to*au, &
      lambda_eff(iband), band_eff(iband), calib(iband), mag(i,k))

    mag(i,k) = mag(i,k) + zero(k)

    if (debug) then
      alpha = acos(dot_product(n_ts,n_to))
      H0 = mag(i,k) - 5.d0*log10(d_ts) - 5.d0*log10(d_to)

      write(iu,*) t_BIN(i,k), mag(i,k), iband, lite, alpha/deg, H0
    endif

  enddo  ! i
enddo  ! k

if (debug) then
  write(iu,*)
endif

! analytical zero point(s)

if (use_zero) then
  do k = 1,nband
    j1 = m_OBS(k)
    j2 = m_BIN(k)
    zero2 = sum(mag_OBS(1:j1,k))/j1 - sum(mag(1:j2,k))/j2
    mag(1:j1,k) = mag(1:j1,k) + zero2
    if (debug) then
      write(*,*) '# zero2(', k, ') = ', zero2, ' mag'
    endif
  enddo
endif

!
! interpolate synthetic lightcurve to the exact times of observations
!

if (debug) then
  open(unit=iu,file="chi2_LC2.dat",status="unknown")
  write(iu,*) "# t_OBS & mag_interp & sigma_mag_OBS & iband & dataset & chi^2"
  write(iu,*) "# t_OBS & mag_OBS    & sigma_mag_OBS & iband & dataset & chi^2"
endif

n = 0
chi2 = 0.d0
do k = 1, nband

  j = 2
  do i = 1, m_OBS(k)

    do while ((j.lt.m_BIN(k)).and.(t_BIN(j,k).le.t_OBS(i,k)))
      j = j + 1
    enddo

    if (m_BIN(k).gt.1) then
      mag_interp = interp(t_BIN(j-1,k), t_BIN(j,k), mag(j-1,k), mag(j,k), t_OBS(i,k))
    else
      mag_interp = mag(1,k)
    endif

    chi2_ = ((mag_interp-mag_OBS(i,k))/sigma_mag_OBS(i,k))**2
    lns = lns + log(sigma_mag_OBS(i,k))
    chi2 = chi2 + chi2_
    n = n + 1
   
    if (debug) then
      write(iu,*) t_OBS(i,k), mag_interp, sigma_mag_OBS(i,k), iband_LC(k), dataset(i,k), chi2_
      write(iu,*) t_OBS(i,k), mag_OBS(i,k), sigma_mag_OBS(i,k), iband_LC(k), dataset(i,k), chi2_
      write(iu,*)
    endif

  enddo  ! i

enddo  ! k

if (debug) then
  close(iu)
endif

return
end subroutine chi2_func_LC2

end module chi2_func_LC2_module


