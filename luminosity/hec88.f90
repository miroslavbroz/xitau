! hec88.f90
! Use Harmanec (1988) relations to constrain the radius, mass,
!   luminosity, or log g of normal Main-Sequence  stars.
! Miroslav Broz (miroslav.broz@email.cz), May 24 2017

! Reference:
!   Harmanec P.: Stellar masses and radii based on modern binary data.
!   Bull. Astron. Inst. Czechosl. 39, 329-345, 1988.

! Note: Inverted logM(logT)

module hec88_module

double precision :: logM

contains

double precision function logM_func(X)
implicit none
double precision, intent(in) :: X
logM_func = (((-1.744951d0*X + 30.31681d0)*X - 196.2387d0)*X + 562.6774d0)*X - 604.0760d0 - logM
return
end function logM_func

subroutine hec88(M, T, R, L, logg)
use root_module
implicit none
double precision, intent(in) :: M
double precision, intent(out) :: T, R, L, logg

! M    ... mass [M_S]
! T    ... effective temperature [K]
! R    ... radius [R_S]
! L    ... luminosity [L_S]
! logg ... radius [cgs]

double precision, parameter :: Mbol_Sun = 4.69d0  ! mag; Popper (1980)

double precision :: X, logT, logR, Mbol

logM = log10(M)

logT = root(logM_func, 3.0d0, 5.0d0)

X = logT

logR = (((-0.8656627d0*X + 16.22018d0)*X - 112.2303d0)*X + 341.6602d0)*X - 387.0969d0

Mbol = (((4.328314d0*X - 81.10091d0)*X + 561.1516d0)*X - 1718.301d0)*X + 1977.795d0

logg = 4.438d0 + logM - 2.d0*logR

T = 10.d0**logT
R = 10.d0**logR
L = 10.d0**(0.4d0*(Mbol_Sun-Mbol))

return
end subroutine hec88

end module hec88_module


