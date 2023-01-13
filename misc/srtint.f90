! srtint.f
! Minimal sort of array; with output of indices.
! Miroslav Broz (miroslav.broz@email.cz), Nov 15th 2009

module srtint_module

contains

subroutine srtint(x, id)

implicit none
integer, dimension(:) :: x
integer, dimension(:) :: id

integer :: j,k,n,min,tmp

n = size(x)
do j = 1, n
  id(j) = j
enddo
do j = 1, n
  min = j
  do k = j+1, n
    if (x(id(min)).gt.x(id(k))) then
      min = k
    endif
  enddo
  tmp = id(j)
  id(j) = id(min)
  id(min) = tmp
enddo

return
end subroutine srtint

end module srtint_module


