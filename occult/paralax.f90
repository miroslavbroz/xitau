! paralax.f90
! Annual paralax correction of RA, DE coordinates.
! Miroslav Broz (miroslav.broz@email.cz), Jun 17th 2022

! Reference:
! Wolf et al. (1992) Astronomicka prirucka. Praha: Academia.

module paralax_module

contains

subroutine paralax(ra, de, pi_, ra_S, de_S, dra, dde)

implicit none
double precision, intent(in) :: ra, de, pi_, ra_S, de_S
double precision, intent(out) :: dra, dde

double precision :: X, Y, Z

X = cos(ra_S)*cos(de_S)
Y = sin(ra_S)*cos(de_S)
Z = sin(de_S)

dra = pi_*(Y*cos(ra) - X*sin(ra))/cos(de)
dde = pi_*(Z*cos(de) - X*cos(ra)*sin(de) - Y*sin(ra)*sin(de))

return
end subroutine paralax

end module paralax_module

