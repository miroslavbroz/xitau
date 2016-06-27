
      REAL*8 FUNCTION amotry(p,y,psum,mp,np,ndim,funk,ihi,fac)

      implicit none
      INTEGER ihi,mp,ndim,np,NMAX
      REAL*8 fac,p(mp,np),psum(np),y(mp),funk
      PARAMETER (NMAX=60)
      EXTERNAL funk
C USES funk

c Extrapolates by a factor fac through the face of the simplex across from the high point,
c tries it, and replaces the high point if the new point is better.

      INTEGER j
      REAL*8 fac1,fac2,ytry,ptry(NMAX)
      fac1=(1.d0-fac)/ndim
      fac2=fac1-fac
      do 11 j=1,ndim
        ptry(j)=psum(j)*fac1-p(ihi,j)*fac2
11    enddo
      ytry=funk(ptry) ! Evaluate the function at the trial point.
      if (ytry.lt.y(ihi)) then ! If it’s better than the highest, then replace the highest.
        y(ihi)=ytry
        do 12 j=1,ndim
          psum(j)=psum(j)-p(ihi,j)+ptry(j)
          p(ihi,j)=ptry(j)
12      enddo
      endif
      amotry=ytry
      return
      END


