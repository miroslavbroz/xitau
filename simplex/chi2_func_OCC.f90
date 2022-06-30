! chi2_func_OCC.f90
! Calculate occultations (no chi^2).
! Miroslav Broz (miroslav.broz@email.cz), Jun 22nd 2022

module chi2_func_OCC_module

contains

subroutine chi2_func_OCC(NOUT, tout, rh, chi2, n)

use const_module
use normalize_module
use rotate_module
use paralax_module
use write_kml_module
use fplane_module
use spath_module
use occult_module

implicit none
include 'simplex.inc'
include 'dependent.inc'

integer, intent(in) :: NOUT
double precision, dimension(OUTMAX), intent(in) :: tout
double precision, dimension(OUTMAX,NBODMAX,3), intent(in) :: rh
double precision, intent(out) :: chi2
integer, intent(out) :: n

! observational data
integer, save :: m_OCC = 0
double precision, dimension(OCCMAX), save :: t, sigma, lambda_obs, phi_obs, alt
double precision, dimension(OCCMAX), save :: alpha, delta, prlx, pmra, pmde, epoch, offra, offde
integer, dimension(OCCMAX), save :: contact, dataset

double precision, parameter :: duration = 0.05d0  ! d

! internal variables
integer, save :: i1st = 0
integer, parameter :: iu = 21, iub = 22, iuc = 23
integer :: i, j, k, l_
integer :: j1, j2, j3, j4
integer :: N_e, N_s, N_o
double precision, dimension(OUTMAX) :: t_e, vardist, ecl, ecb
double precision, dimension(OUTMAX) :: t_s, vardist_s, ecl_s, ecb_s
double precision, dimension(OUTMAX) :: t_o, vardist_o, ecl_o, ecb_o
double precision, dimension(3) :: r, r_, r_AO, r_EA, r_EO
double precision, dimension(3) :: e, axes
double precision, dimension(:,:), pointer :: silh_
double precision :: chi2_
double precision :: l, b, d
double precision :: ra, de, dra, dde, ra_S, de_S
double precision :: eps0
double precision :: lambda, phi
double precision :: lite, t_lite, t_nolite
double precision :: u, v, w
double precision :: xh_interp, yh_interp, zh_interp
logical :: has_solution

! functions
double precision :: interp, interp2, eps_earth

!
! read occultations (only 1st time!)
!
if (i1st.eq.0) then

  call read_OCC(file_OCC, m_OCC, t, sigma, lambda_obs, phi_obs, alt, &
    alpha, delta, prlx, pmra, pmde, epoch, offra, offde, contact, dataset)

  if (debug) then
    write(*,*) '# m_OCC = ', m_OCC
  endif
!
! read ephemerides (Earth==399, Sun==10, occultation)
!
  if (m_OCC.gt.0) then
    call read_ephemeris("ephemeris_E.dat", N_e, t_e, vardist, ecl, ecb)
    call read_ephemeris("ephemeris_10_399.dat", N_s, t_s, vardist_s, ecl_s, ecb_s)
    call read_ephemeris("ephemeris_occ.dat", N_o, t_o, vardist_o, ecl_o, ecb_o)
  endif

  i1st = 1

endif  ! i1st

!
! compute occultations
!
if (debug) then
  open(unit=iu, file='occultation.dat', status='unknown')
  write(iu,*) '# jd [TDB,nolite] & lambda [deg,wgs84] & phi [deg] & ibod'

  open(unit=iub, file='occultation2.dat', status='unknown')
  write(iub,*) '# jd [TDB,nolite] & u [au] & v [au] & ibod'

  open(unit=iuc, file='occultation3.dat', status='unknown')
  write(iuc,*) '# jd [TDB,nolite] & lambda [deg,wgs84] & phi [deg] & ibod'
endif

! Notation:
!
! E .. Earth
! S .. Sun
! A .. asteroid
! O .. occulted *

axes = (/R_E, R_E, R_P/)/au
eps0 = eps_earth(J2000)

do i = 1, m_OCC
  do j = 1, nbod

    j1 = 2
    j2 = 2
    j3 = 2
    j4 = 2

    do k = 0, OCCMAX2

      t_nolite = t(i) + duration*(dble(k-OCCMAX2/2)/OCCMAX2)
!
! interpolate Earth (topocentric, airless, ecliptic, J2000)
!
      do while ((j1.lt.N_e).and.(t_e(j1).le.t_nolite))
        j1 = j1+1
      enddo

      l = interp2(t_e(j1-1), t_e(j1), ecl(j1-1), ecl(j1), t_nolite)
      b = interp(t_e(j1-1), t_e(j1), ecb(j1-1), ecb(j1), t_nolite)
      d = interp(t_e(j1-1), t_e(j1), vardist(j1-1), vardist(j1), t_nolite)
!
! interpolate moons (ecliptic, J2000)
!
      lite = -d/clight * AU/day 
      t_lite = t_nolite + lite

      do while ((j2.lt.NOUT).and.(tout(j2).le.t_lite))
        j2 = j2+1
      enddo

      xh_interp = interp(tout(j2-1), tout(j2), rh(j2-1,j,1), rh(j2,j,1), t_lite)  ! j-th body, x coordinate
      yh_interp = interp(tout(j2-1), tout(j2), rh(j2-1,j,2), rh(j2,j,2), t_lite)  ! y coordinate
      zh_interp = interp(tout(j2-1), tout(j2), rh(j2-1,j,3), rh(j2,j,3), t_lite)  ! z coordinate

      ! ecliptic J2000 -> equatorial J2000
      call uvw(t_nolite, l, b, xh_interp, yh_interp, zh_interp, u, v, w)

      ! au -> rad
      u = u/d
      v = v/d
!
! interpolate Sun (ecliptic, J2000)
!
      do while ((j3.lt.N_s).and.(t_s(j3).le.t_nolite))
        j3 = j3+1
      enddo

      l = interp2(t_s(j3-1), t_s(j3), ecl_s(j3-1), ecl_s(j3), t_nolite)
      b = interp(t_s(j3-1), t_s(j3), ecb_s(j3-1), ecb_s(j3), t_nolite)

      ! ecliptic J2000 -> equatorial J2000
      r = (/cos(l)*cos(b), sin(l)*cos(b), sin(b)/)
      r_ = rot_x(r, cos(eps0), sin(eps0))

      ra_S = atan2(r_(2), r_(1))
      de_S = asin(r_(3))
!
! interpolate star (parallax, proper motion, equatorial, J2000)
!
      call paralax(alpha(i), delta(i), prlx(i), ra_S, de_S, dra, dde)

      ra = alpha(i) + pmra(i)*(t_nolite-epoch(i)) + dra
      de = delta(i) + pmde(i)*(t_nolite-epoch(i)) + dde

      r_EO = (/cos(ra)*cos(de), sin(ra)*cos(de), sin(de)/)
      d = 1.d0/(prlx(i)/arcsec)*pc/au
      r_EO = d*r_EO
!
! interpolate occultation (orbit, offset, ecliptic, J2000)
!
      do while ((j4.lt.N_o).and.(t_o(j4).le.t_nolite))
        j4 = j4+1
      enddo

      l = interp2(t_o(j4-1), t_o(j4), ecl_o(j4-1), ecl_o(j4), t_nolite)
      b = interp(t_o(j4-1), t_o(j4), ecb_o(j4-1), ecb_o(j4), t_nolite)
      d = interp(t_o(j4-1), t_o(j4), vardist_o(j4-1), vardist_o(j4), t_nolite)

      ! ecliptic J2000 -> equatorial J2000
      r = (/cos(l)*cos(b), sin(l)*cos(b), sin(b)/)
      r_ = rot_x(r, cos(eps0), sin(eps0))

      ! u .. -RA
      ! v .. +DE
      ra = atan2(r_(2), r_(1)) - u + offra(i)
      de = asin(r_(3)) + v + offde(i)

      r_EA = (/cos(ra)*cos(de), sin(ra)*cos(de), sin(de)/)
      r_EA = d*r_EA

      r_AO = r_EA + r_EO
      r_AO = normalize(r_AO)

      call occult(t_nolite, r_EA, r_AO, e, axes, lambda, phi, has_solution)

      if (has_solution) then
        write(iu,*) t_nolite, lambda/deg, phi/deg, j
        call to_kml(lambda, phi, j)
      else
        write(iu,*) t_nolite, ' ?', ' ?', j
      endif
!
! fundamental plane
!
      call fplane(r_EA, r_AO, r_EO, u, v, has_solution)

      if (has_solution) then
        write(iub,*) t_nolite, u, v, j
      endif
!
! shadow path
!
      if ((j.eq.1).and.(mod(k,5).eq.0)) then

        call spath(t_nolite, lite, r_EA, r_AO, e, axes, l, b, silh_, has_solution)

        if (has_solution) then
          do l_ = 1, size(silh_, 1)
            if (silh_(l_, 1).ne.NAN) then
              write(iuc,*) t_nolite, silh_(l_, :)/deg, j
            else
              write(iuc,*) t_nolite, ' ?', ' ?', j
            endif
          enddo
          write(iuc,*)
          write(iuc,*)
        endif
      endif

    enddo  ! k, OCCMAX2

    write(iu,*)
    write(iu,*)
    write(iub,*)
    write(iub,*)

  enddo  ! j, nbod
enddo  ! i, m_OCC

if (debug) then
  close(iu)
  close(iub)
  close(iuc)
endif

call write_kml('occultation.kml')

chi2 = 0.d0
n = 0

return
end subroutine chi2_func_OCC

end module chi2_func_OCC_module


