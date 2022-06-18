! occult.f90
! Occultation computation (star -> asteroid -> Earth).
! Miroslav Broz (miroslav.broz@email.cz), Jun 9th 2022

module occult_module

contains

subroutine occult(t_TDB, r_EA, r_AS, e, axes, lambda, phi, t_UT1, has_solution)

use intersect_AB_e_module
use const_module
use rotate_module
use lst_module
use preces_module
use nutate_module
use geodetic_module

implicit none
double precision, intent(in) :: t_TDB
double precision, dimension(3), intent(in) :: r_EA, r_AS
double precision, dimension(3), intent(out) :: e, axes
double precision, intent(out) :: lambda, phi, t_UT1
logical, intent(out) :: has_solution

integer :: j
double precision :: abse
double precision, dimension(3) :: e_, e__
double precision :: TDB1, TDB2, DTDB
double precision :: UT11, UT12, DUT1
double precision :: TT1, TT2
double precision :: TAI1, TAI2
double precision :: elong, u, v
double precision :: GST
double precision :: ERA
double precision :: t0, t, zeta, zz, theta
double precision :: eps, deltapsi, deltaeps
double precision :: s0, ss, s
double precision :: eps_earth
double precision :: iau_dtdb, iau_gst06a, iau_era00

call intersect_AB_e(r_EA, -r_AS, e, axes, has_solution)

if (.not.has_solution) then
  return
endif

TDB1 = t_TDB
TDB2 = 0.d0
UT12 = 0.d0
elong = 0.d0
u = 0.d0
v = 0.d0

DUT1 = 0.4d0 ! UT1-UTC; https://datacenter.iers.org/data/17/bulletind-081.txt

DTDB = iau_dtdb(TDB1, TDB2, UT12, elong, u, v)
call iau_tdbtt(TDB1, TDB2, DTDB, TT1, TT2, j)
call iau_tttai(TT1, TT2, TAI1, TAI2, j)
call iau_taiut1(TAI1, TAI2, DUT1, UT11, UT12, j)
ERA = iau_era00(UT11, UT12)
!GMST = iau_gmst06(UT11, UT12, TT1, TT2)
GST = iau_gst06a(UT11, UT12, TT1, TT2)
t_UT1 = UT11+UT12

e_ = e  ! equatorial J2000 -> equatorial J2000

! precession, nutation
t0 = 0.d0
t = (t_UT1-J2000)/36525.d0
eps = eps_earth(t_UT1)

call preces_angle(t0,t,zeta,zz,theta)
call preces2(e_,zeta,zz,theta)
call nutate_angle(t,deltapsi,deltaeps)
call nutate2(e_,eps,deltapsi,deltaeps)

! Earth rotation
lambda = 0.d0
call lst(t_UT1,lambda,eps,deltapsi,s0,ss,s)
e__ = rot_z(e_, cos(s), sin(s))

call geodetic(e__, axes, lambda, phi)

return
end subroutine occult

end module occult_module

 
