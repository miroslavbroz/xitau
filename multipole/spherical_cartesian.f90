! spherical_cartesian.f90
! Convert from spherical to cartesian coordinates (suitable for accelerations).
! Miroslav Broz (miroslav.broz@email.cz), Aug 21st 2020

module spherical_cartesian_module

contains

function spherical_cartesian(r, a)

double precision, dimension(3) :: spherical_cartesian, r, a
double precision, dimension(3) :: tmp
double precision :: sintheta, costheta, sinphi, cosphi

!a(2) = 1.d0
!a(3) = 0.5d0
!write(*,*) 'a_g = ', a, ' m s^-2 (spherical)'
!write(*,*) 'r = ', r, ' m (spherical)'

sintheta = sin(r(2))
costheta = cos(r(2))
sinphi = sin(r(3))
cosphi = cos(r(3))

tmp(1) = a(1)*sintheta*cosphi - a(2)*costheta*cosphi - a(3)*sinphi
tmp(2) = a(1)*sintheta*sinphi - a(2)*costheta*sinphi + a(3)*cosphi
tmp(3) = a(1)*costheta        + a(2)*sintheta

spherical_cartesian = tmp
return

end function spherical_cartesian

end module spherical_cartesian_module

