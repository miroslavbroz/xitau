! jacobi.f90

module jacobi_module

contains

SUBROUTINE jacobi(a,d,v,nrot)
USE nrtype; USE nrutil !, ONLY : assert_eq,get_diag,nrerror,unit_matrix,upper_triangle
IMPLICIT NONE
INTEGER(I4B), INTENT(OUT) :: nrot
REAL(DP), DIMENSION(:), INTENT(OUT) :: d
REAL(DP), DIMENSION(:,:), INTENT(INOUT) :: a
REAL(DP), DIMENSION(:,:), INTENT(OUT) :: v
! Computes all eigenvalues and eigenvectors of a real symmetric N × N matrix a. On output,
! elements of a above the diagonal are destroyed. d is a vector of length N that returns the
! eigenvalues of a. v is an N × N matrix whose columns contain, on output, the normalized
! eigenvectors of a . nrot returns the number of Jacobi rotations that were required.
INTEGER(I4B) :: i,ip,iq,n
REAL(DP) :: c,g,h,s,sm,t,tau,theta,tresh
REAL(DP), DIMENSION(size(d)) :: b,z

n=assert_eq((/size(a,1),size(a,2),size(d),size(v,1),size(v,2)/),'jacobi')
call unit_matrix(v(:,:))   ! Initialize v to the identity matrix.
b(:)=get_diag_dv(a(:,:))  ! Initialize b and d to the diagonal of a.
d(:)=b(:)
z(:)=0.0   ! This vector will accumulate terms of the form tapq as in eq. (11.1.14).
nrot=0
do i=1,50
  sm=sum(abs(a),mask=upper_triangle(n,n))   ! Sum off-diagonal elements.
  if (sm == 0.0d0) RETURN  ! The normal return, which relies on quadratic convergence to machine underflow.
  tresh=merge(0.2_dp*sm/n**2,0.0_dp, i < 4 )  ! On the first three sweeps, we will rotate only if tresh exceeded.
  do ip=1,n-1
    do iq=ip+1,n
      g=100.0_dp*abs(a(ip,iq))  ! After four sweeps, skip the rotation if the off-diagonal element is small.
      if ((i > 4) .and. (abs(d(ip))+g == abs(d(ip))) .and. (abs(d(iq))+g == abs(d(iq)))) then
        a(ip,iq)=0.0
      else if (abs(a(ip,iq)) > tresh) then
        h=d(iq)-d(ip)
        if (abs(h)+g == abs(h)) then
          t=a(ip,iq)/h  ! t = 1/(2θ)
        else
          theta=0.5_dp*h/a(ip,iq)   ! Equation (11.1.10).
          t=1.0_dp/(abs(theta)+sqrt(1.0_dp+theta**2))
          if (theta < 0.0) t=-t
        end if
        c=1.0_dp/sqrt(1+t**2)
        s=t*c
        tau=s/(1.0_dp+c)
        h=t*a(ip,iq)
        z(ip)=z(ip)-h
        z(iq)=z(iq)+h
        d(ip)=d(ip)-h
        d(iq)=d(iq)+h
        a(ip,iq)=0.0
        call jrotate(a(1:ip-1,ip),a(1:ip-1,iq))        ! Case of rotations 1 ≤ j < p.
        call jrotate(a(ip,ip+1:iq-1),a(ip+1:iq-1,iq))  ! Case of rotations p < j < q.
        call jrotate(a(ip,iq+1:n),a(iq,iq+1:n))        ! Case of rotations q < j ≤ n.
        call jrotate(v(:,ip),v(:,iq))
        nrot=nrot+1
      end if
    end do
  end do
  b(:)=b(:)+z(:)
  d(:)=b(:)  ! Update d with the sum of tapq ,
  z(:)=0.0   ! and reinitialize z.
end do
call nrerror('too many iterations in jacobi')

CONTAINS

SUBROUTINE jrotate(a1,a2)
IMPLICIT NONE
REAL(DP), DIMENSION(:), INTENT(INOUT) :: a1,a2
REAL(DP), DIMENSION(size(a1)) :: wk1
wk1(:)=a1(:)
a1(:)=a1(:)-s*(a2(:)+a1(:)*tau)
a2(:)=a2(:)+s*(wk1(:)-a2(:)*tau)
END SUBROUTINE jrotate

END SUBROUTINE jacobi

end module jacobi_module



