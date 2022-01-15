! normalize.f90
! Normalize vector.
! Miroslav Broz (miroslav.broz@email.cz), Aug 7th 2020

module normalize_module

contains

function normalize(a)

implicit none

double precision, dimension(3) :: normalize
double precision, dimension(3), intent(in) :: a

normalize = a/sqrt(dot_product(a,a))

end function normalize

end module normalize_module


