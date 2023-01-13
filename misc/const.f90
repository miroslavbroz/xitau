! const.f90
! Constants.
! Miroslav Broz (miroslav.broz@email.cz), Nov 18th 2019

module const_module

double precision, parameter :: pi = 4.0d0*atan(1.d0), deg = pi/180.d0, rad = 180.d0/pi
double precision, parameter :: arcsec = deg/3600.d0
double precision, parameter :: mas = 1.d-3*arcsec

double precision, parameter :: clight = 2.99792458d8  ! m/s, from CODATA 2018, exact
double precision, parameter :: G = 6.67430d-11  ! kg^-1 m^3 s^-2, CODATA 2018; https://physics.nist.gov/cgi-bin/cuu/Value?bg
double precision, parameter :: GM_S = 0.2959122082855911d-03  ! AU^3/day^2, from JPLEPH DE405
double precision, parameter :: au = 1.49597870700d11  ! m, from IAU 2012
double precision, parameter :: day = 86400.d0  ! s
double precision, parameter :: pc = 648000.d0/pi*au  ! m, from IAU 2015

double precision, parameter :: h_P = 6.62607015d-34  ! J s, from 2018 CODATA, exact
double precision, parameter :: k_B = 1.380649d-23  ! J K^-1, from 2018 CODATA, exact

!double precision, parameter :: M_S = 1.989d30  ! kg
double precision, parameter :: M_S = GM_S*au**3/day**2/G
double precision, parameter :: R_S = 6.957d8  ! m; from IAU 2015
double precision, parameter :: R_E = 6.378173d6  ! m; WGS-82 http://wiki.gis.com/wiki/index.php/Reference_ellipsoid 
double precision, parameter :: R_P = 6.3567523142d6  ! m; WGS-84 http://wiki.gis.com/wiki/index.php/Reference_ellipsoid

double precision, parameter :: J2000 = 2451545.d0  ! d

end module const_module


