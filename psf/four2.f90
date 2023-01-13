module four2_module
contains

SUBROUTINE four2(data,isign)
USE nrtype
!USE nr, ONLY : fourrow
use fourrow_module
IMPLICIT NONE
COMPLEX(DPC), DIMENSION(:,:), INTENT(INOUT) :: data
INTEGER(I4B), INTENT(IN) :: isign
! Replaces a 2-d complex array data by its discrete 2-d Fourier transform, if isign is input
! as 1; or replaces data by its inverse 2-d discrete Fourier transform times the product of its
! two sizes, if isign is input as −1. Both of data’s sizes must be integer powers of 2(thi s
! is checked for in fourrow). Parallelism is by use of fourrow.
COMPLEX(DPC), DIMENSION(size(data,2),size(data,1)) :: temp

call fourrow(data,isign) ! Transform in second dimension.
temp=transpose(data)     ! Tranpose.
call fourrow(temp,isign) ! Transform in (original) first dimension.
data=transpose(temp)     ! Transpose into data.
END SUBROUTINE four2

end module four2_module


