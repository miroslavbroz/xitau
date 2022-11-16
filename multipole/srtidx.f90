! srtidx.f
! Minimal sort of array; with output od indices.
! Miroslav Broz (miroslav.broz@email.cz), Nov 15th 2009

module srtidx_module

contains

subroutine srtidx(x, id)

implicit none
double precision, dimension(:) :: x
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
end subroutine srtidx

end module srtidx_module


