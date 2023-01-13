! write_poles.f
! Write pole orientations.
! Miroslav Broz (miroslav.broz@email.cz), Apr 20th 2022

module write_poles_module

contains

subroutine write_poles(t, ecl, ecb, l, b, vardist, dataset)

implicit none
include 'chi2.inc'
include 'dependent.inc'

integer :: dataset
double precision :: t, ecl, ecb, l, b, vardist

real*8 n_cp(3)
integer, parameter :: iu=15
integer, save :: i1st=0

if (.not.debug) return  ! dbg
!if (.not.debug_swift) return

! centre-pole vector
n_cp = (/cos(l)*cos(b), sin(l)*cos(b), sin(b)/)

call uvw(t, ecl, ecb, n_cp(1), n_cp(2), n_cp(3), n_cp(1), n_cp(2), n_cp(3))

if (i1st.eq.0) then
  open(unit=iu,file="poles.dat",status="unknown")
  write(iu,*) '# dataset x y z vardist'
  write(iu,*) '# - 1 1 1 au'
  i1st = 1
else
  open(unit=iu,file="poles.dat",access="append")
endif
write(iu,*) dataset,0.d0,0.d0,0.d0,vardist
write(iu,*) dataset,n_cp(1),n_cp(2),n_cp(3),vardist
write(iu,*)
write(iu,*)
close(iu)

return
end subroutine write_poles

end module write_poles_module


