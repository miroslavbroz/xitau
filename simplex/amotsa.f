      REAL*8 FUNCTION amotsa(p,y,psum,mp,np,ndim,pb,yb,funk,ihi,yhi,fac)
      implicit none
      INTEGER ihi,mp,ndim,np,NMAX
      REAL*8 fac,yb,yhi,p(mp,np),pb(np),psum(np),y(mp),funk
      PARAMETER (NMAX=200)
      EXTERNAL funk
c USES funk,ran1
c      Extrapolates by a factor fac through the face of the simplex across from the high point,
c      tries it, and replaces the high point if the new point is better.
      INTEGER idum,j
      REAL*8 fac1,fac2,tt,yflu,ytry,ptry(NMAX),ran1
      COMMON /ambsa/ tt,idum
      fac1=(1.d0-fac)/ndim
      fac2=fac1-fac
      do j=1,ndim
        ptry(j)=psum(j)*fac1-p(ihi,j)*fac2
      enddo
      ytry=funk(ptry)
      if (ytry.le.yb) then             ! Save the best-ever.
        do j=1,ndim
          pb(j)=ptry(j)
        enddo
        yb=ytry
      endif
      yflu=ytry-tt*log(ran1(idum))     ! We added a thermal fluctuation to all the current vertices,
      if (yflu.lt.yhi) then            !    but we subtract it here, so as to give the simplex
        y(ihi)=ytry                  !    a thermal Brownian motion: It likes to accept any
        yhi=yflu                     !    suggested change.
        do j=1,ndim
          psum(j)=psum(j)-p(ihi,j)+ptry(j)
          p(ihi,j)=ptry(j)
        enddo
      endif
      amotsa=yflu
      return
      END


