module fourrow_module
contains

SUBROUTINE fourrow(data,isign)
USE nrtype; USE nrutil, ONLY : assert,swap
IMPLICIT NONE
COMPLEX(DPC), DIMENSION(:,:), INTENT(INOUT) :: data
INTEGER(I4B), INTENT(IN) :: isign
! Replaces each row (constant first index) of data(1:M,1:N) by its discrete Fourier transform
! (transform on second index), if isign is input as 1; or replaces each row of data
! by N times its inverse discrete Fourier transform, if isign is input as −1. N must be an
! integer power of 2. Parallelism is M-fold on the first index of data.
INTEGER(I4B) :: n,i,istep,j,m,mmax,n2
REAL(DP) :: theta
COMPLEX(DPC), DIMENSION(size(data,1)) :: temp
COMPLEX(DPC) :: w,wp ! Double precision for the trigonometric recurrences.
COMPLEX(DPC) :: ws

n=size(data,2)
call assert(iand(n,n-1)==0, 'n must be a power of 2 in fourrow_sp')
n2=n/2
j=n2
! This is the bit-reversal section of the routine.
do i=1,n-2
  if (j > i) call swap(data(:,j+1),data(:,i+1))
  m=n2
  do
    if (m < 2 .or. j < m) exit
    j=j-m
    m=m/2
  end do
  j=j+m
end do
mmax=1
! Here begins the Danielson-Lanczos section of the routine.
do ! Outer loop executed log2 N times.
  if (n <= mmax) exit
  istep=2*mmax
  theta=PI_D/(isign*mmax) ! Initialize for the trigonometric recurrence.
  wp=cmplx(-2.0_dp*sin(0.5_dp*theta)**2,sin(theta),kind=dpc)
  w=cmplx(1.0_dp,0.0_dp,kind=dpc)
  do m=1,mmax ! Here are the two nested inner loops.
    ws=w
    do i=m,n,istep
      j=i+mmax
      temp=ws*data(:,j) ! This is the Danielson-Lanczos formula.
      data(:,j)=data(:,i)-temp
      data(:,i)=data(:,i)+temp
    end do
    w=w*wp+w ! Trigonometric recurrence.
  end do
  mmax=istep
end do
END SUBROUTINE fourrow

end module fourrow_module

