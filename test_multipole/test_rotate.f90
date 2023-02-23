! test_rotate.f90

program test_rotate

use const_module
use rotate_module

double precision, dimension(3) :: a, b
double precision :: c

a = (/1.d0, 0.d0, 0.d0/)
b = (/0.d0, 1.d0, 0.d0/)
b = (/0.d0, 0.d0, 1.d0/)
b = (/1.d0, 0.d0, 0.d0/)
b = (/0.d0, -1.d0, 0.d0/)

c = 45*deg

a = vaxis_rotate(a, b, c)

end program test_rotate


