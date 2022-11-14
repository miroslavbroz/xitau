! c1.inc
! Copy polygons.
! Miroslav Broz (miroslav.broz@email.cz), Nov 3rd 2022

! Note: This is suitable for include '...', which is faster!

poly_i%c = poly_k%c
do k = 1, poly_k%c
  l = poly_k%s(k)%c
  poly_i%s(k)%c = l
  poly_i%s(k)%p(1:l,:) = poly_k%s(k)%p(1:l,:)
enddo


