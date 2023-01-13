module realft2_module
contains

SUBROUTINE realft2(data,spec,speq,isign)
USE nrtype; USE nrutil, ONLY : assert,assert_eq
use four2_module
REAL(DP), DIMENSION(:,:), INTENT(INOUT) :: data
COMPLEX(DPC), DIMENSION(:,:), INTENT(INOUT) :: spec
COMPLEX(DPC), DIMENSION(:), INTENT(INOUT) :: speq
INTEGER(I4B), INTENT(IN) :: isign
! Given a two-dimensional real array data(1:M,1:N), this routine returns (for isign=1)
! the complex fast Fourier transform as two complex arrays: On output, spec(1:M/2,1:N)
! contains the zero and positive frequency values of the first frequency component, while
! speq(1:N) contains the Nyquist critical frequency values of the first frequency component.
! The second frequency components are stored for zero, positive, and negative frequencies,
! in standard wrap-around order. For isign=-1, the inverse transform (times M × N/2 as
! a constant multiplicative factor) is performed, with output data deriving from input spec
! and speq. For inverse transforms on data not generated first by a forward transform, make
! sure the complex input data array satisfies property (12.5.2). The size of all arrays must
! always be integer powers of 2.
INTEGER :: i1,j1,nn1,nn2
REAL(DP) :: theta
COMPLEX(DPC) :: c1=(0.5_dp,0.0_dp),c2,h1,h2,w
COMPLEX(DPC), DIMENSION(size(data,2)-1) :: h1a,h2a
COMPLEX(DPC) :: ww,wp

nn1=assert_eq(size(data,1),2*size(spec,1),'rlft2: nn1')
nn2=assert_eq(size(data,2),size(spec,2),size(speq),'rlft2: nn2')
call assert(iand((/nn1,nn2/),(/nn1,nn2/)-1)==0, &
  'dimensions must be powers of 2 in rlft2')
c2=cmplx(0.0_dp,-0.5_dp*isign,kind=dpc)
theta=TWOPI_D/(isign*nn1)
wp=cmplx(-2.0_dp*sin(0.5_dp*theta)**2,sin(theta),kind=dpc)
if (isign == 1) then ! Case of forward transform.
  spec(:,:)=cmplx(data(1:nn1:2,:),data(2:nn1:2,:),kind=dpc)
  call four2(spec,isign) ! Here is where most all of the compute time
  speq=spec(1,:)         ! is spent.
end if
h1=c1*(spec(1,1)+conjg(speq(1)))
h1a=c1*(spec(1,2:nn2)+conjg(speq(nn2:2:-1)))
h2=c2*(spec(1,1)-conjg(speq(1)))
h2a=c2*(spec(1,2:nn2)-conjg(speq(nn2:2:-1)))
spec(1,1)=h1+h2
spec(1,2:nn2)=h1a+h2a
speq(1)=conjg(h1-h2)
speq(nn2:2:-1)=conjg(h1a-h2a)
ww=cmplx(1.0_dp,0.0_dp,kind=dpc) ! Initialize trigonometric recurrence.
do i1=2,nn1/4+1
  j1=nn1/2-i1+2 ! Corresponding negative frequency.
  ww=ww*wp+ww   ! Do the trig recurrence.
  w=ww
  h1=c1*(spec(i1,1)+conjg(spec(j1,1))) ! Equation (12.3.5).
  h1a=c1*(spec(i1,2:nn2)+conjg(spec(j1,nn2:2:-1)))
  h2=c2*(spec(i1,1)-conjg(spec(j1,1)))
  h2a=c2*(spec(i1,2:nn2)-conjg(spec(j1,nn2:2:-1)))
  spec(i1,1)=h1+w*h2
  spec(i1,2:nn2)=h1a+w*h2a
  spec(j1,1)=conjg(h1-w*h2)
  spec(j1,nn2:2:-1)=conjg(h1a-w*h2a)
end do
if (isign == -1) then ! Case of reverse transform.
  call four2(spec,isign)
  data(1:nn1:2,:)=real(spec)
  data(2:nn1:2,:)=aimag(spec)
end if
END SUBROUTINE realft2

end module realft2_module

