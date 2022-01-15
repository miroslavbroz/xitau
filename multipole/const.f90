! const.f90
! Constants.
! Miroslav Broz (miroslav.broz@email.cz), Nov 18th 2019

module const_module

double precision, parameter :: pi = 4.0d0*atan(1.d0), deg = pi/180.d0, rad = 180.d0/pi

double precision, parameter :: G = 6.67430d-11  ! kg^-1 m^3 s^-2, CODATA 2018; https://physics.nist.gov/cgi-bin/cuu/Value?bg
double precision, parameter :: GM_S = 0.2959122082855911d-03  ! AU^3/day^2, from JPLEPH DE405
double precision, parameter :: au = 1.49597870700d11  ! m, from IAU 2012
double precision, parameter :: day = 86400.d0  ! s

!double precision, parameter :: M_S = 1.989d30  ! kg
double precision, parameter :: M_S = GM_S*au**3/day**2/G

end module const_module


