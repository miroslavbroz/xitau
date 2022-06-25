! interp2.f90
! Interpolation of angles (0-360 deg).
! Miroslav Broz (miroslav.broz@email.cz), Jun 24th 2022

module interp2_module

contains

double precision function interp2(x1, x2, y1, y2, x)
use const_module
implicit none
double precision, intent(in) :: x1, x2, y1, y2, x
double precision :: tmp
double precision :: interp
tmp = y1
if ((y2-tmp).gt.180.d0*deg) then
  tmp = tmp + 360.d0*deg
elseif ((y2-tmp).lt.-180.d0*deg) then
  tmp = tmp - 360.d0*deg
endif
interp2 = interp(x1, x2, tmp, y2, x)
return
end function interp2

end module interp2_module


