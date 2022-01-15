! vector_product.f90
! Vector product of two vectors.
! Miroslav Broz (miroslav.broz@email.cz), Jul 31st 2007

module vector_product_module

contains

function vector_product(a, b)

implicit none

double precision, dimension(3) :: vector_product
double precision, dimension(3) :: a, b, c

c(1) = a(2)*b(3) - a(3)*b(2);
c(2) = a(3)*b(1) - a(1)*b(3);
c(3) = a(1)*b(2) - a(2)*b(1);

vector_product = c

return

end function vector_product

end module vector_product_module


