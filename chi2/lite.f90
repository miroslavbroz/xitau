! lite.f90
! Light-time effect.
! Miroslav Broz (miroslav.broz@email.cz), Jan 21st 2025

! +z     2 *...       t            
!          |                       
!          |          lite of 2 '-'
!          V                       
!  0       + c.o.m.   t_interp     
!          ^                       
!          |          lite of 1 '+'
!          |                       
! -z     1 *...       t            
!                                  
!     to observer                  

module lite_module

contains

subroutine lite(nout, tout, rb, vb, rl, vl)

use const_module

implicit none
include 'chi2.inc'
include 'dependent.inc'

integer, intent(in) :: nout
double precision, dimension(OUTMAX), intent(in) :: tout
double precision, dimension(OUTMAX,NBODMAX,3), intent(in) :: rb, vb
double precision, dimension(OUTMAX,NBODMAX,3), intent(out) :: rl, vl

integer, parameter :: iu = 20
integer :: i, j, k, l
double precision :: lite_, delta, t_interp, zb

! functions
double precision, external :: au_day, extrap

if (debug_swift) then
  open(unit=iu,file="lite.dat",status="unknown")
  write(iu,*) '# tout [d] & ibod & lite [d] & delta [d]'
endif

k = 2
do i = 1, nout
  do j = 1, nbod
    t_interp = tout(i)

! iterate light-time effect
    do l = 1, 3

      do while ((k.gt.2).and.(tout(k-1).gt.t_interp))
        k = k-1
      enddo
      do while ((k.lt.nout).and.(tout(k).le.t_interp))
        k = k+1
      enddo
     
      zb = extrap(tout(k-1), tout(k), rb(k-1,j,3), rb(k,j,3), t_interp)

      lite_ = -au_day(zb)
      t_interp = tout(i) + lite_
      delta = abs(t_interp - tout(i) - lite_)
    enddo

! interpolate other coordinates
    do l = 1, 3
      rl(i,j,l) = extrap(tout(k-1), tout(k), rb(k-1,j,l), rb(k,j,l), t_interp)
      vl(i,j,l) = extrap(tout(k-1), tout(k), vb(k-1,j,l), vb(k,j,l), t_interp)
    enddo

    if (debug_swift) then
      write(iu,*) tout(i), j, lite_, delta
    endif

  enddo
enddo

if (debug_swift) then
  close(iu)
endif

! Note: lite is extrapolated for a few 1st and last points!

return
end subroutine lite

end module lite_module


