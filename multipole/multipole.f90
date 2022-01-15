! multipole.f90
! Compute gravitational potential using a multipole expansion.
! Miroslav Broz (miroslav.broz@email.cz), Nov 14th 2019

module multipole_module

implicit none

contains

subroutine multipole(elems, nodes, capm, capr, rho, Clm, Slm)

use factorial_module
use volume_module
use legendre_module

integer, dimension(:,:), pointer :: elems
double precision, dimension(:,:), pointer :: nodes
double precision :: capm, capr, rho
double precision, dimension(0:,0:) :: Clm, Slm

integer :: i,l,m
double precision :: dV, costheta, phi, absr, tmp
double precision, dimension(3) :: a, b, c, d, r

Clm = 0.d0
Slm = 0.d0

do i = 1, size(elems,1)
  a = nodes(elems(i,1),:)
  b = nodes(elems(i,2),:)
  c = nodes(elems(i,3),:)
  d = nodes(elems(i,4),:)

  dV = v1(a,b,c,d)
  r = (a+b+c+d)/4.d0
  absr = sqrt(dot_product(r,r))
  costheta = r(3)/absr
  phi = atan2(r(2),r(1))

  do l = 0, size(Clm,1)-1
    do m = 0, l
      if (m.eq.0) then
        tmp = 1.d0/(capm*capr**l) * rho*(absr**l) * Pl(l,costheta) * dV
        Clm(l,m) = Clm(l,m) + tmp
      else
!        tmp = 1.d0/(2.d0*capm*capr**l) * factorial(l-m)/factorial(l+m) * rho*(absr**l) * Plm(l,m,costheta) * dV  ! Sidlichovsky
        tmp = 2.d0/(capm*capr**l) * factorial(l-m)/factorial(l+m) * rho*(absr**l) * Plm(l,m,costheta) * dV  ! multiplied by 4
        Clm(l,m) = Clm(l,m) + tmp*cos(m*phi)
        Slm(l,m) = Slm(l,m) + tmp*sin(m*phi)
      endif
    enddo
  enddo

enddo

return

end subroutine multipole


double precision function potential_mp(Clm, Slm, npole, capm, capr, r)

use const_module
use legendre_module

implicit none

double precision, dimension(0:,0:) :: Clm, Slm
integer :: npole
double precision :: capm, capr
double precision, dimension(3) :: r

integer l, m
double precision :: absr, costheta, phi, U

U = 0.d0

absr = sqrt(dot_product(r,r))
costheta = r(3)/absr
phi = atan2(r(2),r(1))

do l = 0, npole
  do m = 0, l
    U = U + (capr/absr)**l * Plm(l,m,costheta) * (Clm(l,m)*cos(m*phi) + Slm(l,m)*sin(m*phi))  ! Sidlichovsky; k = GM != m
  enddo
enddo

U = -U*G*capm/absr
potential_mp = U

return

end function potential_mp


function a_g_mp(Clm, Slm, npole, capm, capr, r)

use const_module
use spherical_cartesian_module
use legendre_module

implicit none

double precision, dimension(3) :: a_g_mp
double precision, dimension(0:,0:) :: Clm, Slm
integer :: npole
double precision :: capm, capr
double precision, dimension(3) :: r

integer l, m
double precision :: absr, theta, phi, costheta, sintheta
double precision :: dU_dr, dU_dtheta, dU_dphi, Plm_
double precision, dimension(3) :: a_g

a_g = 0.d0

absr = sqrt(dot_product(r,r))
costheta = r(3)/absr
sintheta = sqrt(1.d0-costheta**2)
theta = acos(costheta)
phi = atan2(r(2),r(1))

! U = G*capm/absr * \sum_l=0^\infty \sum_m=0^l (capr/absr)**l * Plm(l,m,costheta) * (Clm(l,m)*cos(m*phi) + Slm(l,m)*sin(m*phi))  ! Sidlichovsky; k = GM != m

do l = 0, npole
  do m = 0, l

    Plm_ = Plm(l,m,costheta)
    dU_dr = capr**l * (-l-1)*absr**(-l-2) * Plm_ * (Clm(l,m)*cos(m*phi) + Slm(l,m)*sin(m*phi))
    dU_dtheta = capr**l * absr**(-l-1) * dPlm_dx(l,m,costheta,Plm_)*sintheta * (Clm(l,m)*cos(m*phi) + Slm(l,m)*sin(m*phi))  ! minus or NOT?
    dU_dphi = capr**l * absr**(-l-1) * Plm_ * (-Clm(l,m)*sin(m*phi)*m + Slm(l,m)*cos(m*phi)*m)

    a_g(1) = a_g(1) + dU_dr
    a_g(2) = a_g(2) + dU_dtheta/absr
    if (sintheta.ne.0.d0) then
      a_g(3) = a_g(3) + dU_dphi/(absr*sintheta)
    endif
  enddo
enddo

a_g = a_g*G*capm
a_g = spherical_cartesian([absr, theta, phi], a_g)
a_g_mp = a_g

return

end function a_g_mp

end module multipole_module


