! omega_roche_approx.f
! Roche potential corresponding to volume-equivalent radius.
! Miroslav Broz (miroslav.broz@email.cz), Feb 18th 2022

module omega_roche_approx_module

contains

double precision function omega_roche_approx(R, q_, k)

! R ... radius [a=1]
! q ... mass ratio m2/m1
! F ... fill-out factor
! P ... synchronicity
! k ... component

use const_module
use roche_module

implicit none

include '../chi2/chi2.inc'
include '../chi2/dependent.inc'

integer :: k
double precision, intent(in) :: R, q_

integer :: i
integer, parameter :: n = 46
double precision :: V, OmegaR
double precision, save :: qlast(2) = 0.d0
double precision, dimension(n, 2), save :: Rvol, Omega
double precision, parameter :: F1 = 0.1d0, F2 = 1.0d0

! functions
double precision :: interp

! roche_module shared variables: q, F, P
q = q_
P = 1.d0

! precompute R(Omega)
if (q.ne.qlast(k)) then
  do i = 1, n
    F = F1 + (F2-F1)*dble(i-1)/(n-1)
    V = RocheVolume()
    Omega(i,k) = OmegaF
    Rvol(i,k) = (V/(4.d0/3.d0*pi))**(1.d0/3.d0)
  enddo
  qlast(k) = q
endif

! interpolate Omega(R)
if (R.lt.Rvol(1,k)) then

  OmegaR = Omega(1,k)
  if (debug) then
    write(*,*) '# Warning: stellar radius too small in omega_roche_approx!'
  endif

else if (R.gt.Rvol(n,k)) then

  OmegaR = Omega(n,k)  ! L1
  if (debug) then
    write(*,*) '# Warning: stellar radius too BIG in omega_roche_approx!'
    write(*,*) '# R    = ', R  , ' a'
    write(*,*) '# Rvol = ', Rvol(n,k), ' a'
    write(*,*) '# RL1  = ', RL1, ' a'
  endif

else

  i = 2
  do while ((Rvol(i,k).lt.R).and.(i.lt.n))
    i = i+1
  enddo
  OmegaR = interp(Rvol(i-1,k), Rvol(i,k), Omega(i-1,k), Omega(i,k), R)

endif

if (k.eq.2) then
  OmegaR = secondary_correction(OmegaR, q)
endif

omega_roche_approx = OmegaR

return
end function omega_roche_approx

end module omega_roche_approx_module


