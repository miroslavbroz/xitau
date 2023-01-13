
      SUBROUTINE amoeba(p,y,mp,np,ndim,ftol,funk,iter,itmax)

      implicit none
      include '../chi2/chi2.inc'
      INTEGER iter,itmax,mp,ndim,np
      REAL*8 ftol,p(mp,np),y(mp),funk,TINY
      PARAMETER (TINY=1.d-10) ! Maximum allowed dimensions and function
      EXTERNAL funk
C USES amotry,funk

c Multidimensional minimization of the function funk(x) where x(1:ndim) is a vector
c in ndim dimensions, by the downhill simplex method of Nelder and Mead. The matrix
c p(1:ndim+1,1:ndim) is input. Its ndim+1 rows are ndim-dimensional vectors which are
c the vertices of the starting simplex. Also input is the vector y(1:ndim+1), whose components
c must be pre-initialized to the values of funk evaluated at the ndim+1 vertices (rows)
c of p; and ftol the fractional convergence tolerance to be achieved in the function value
c (n.b.!). On output, p and y will have been reset to ndim+1 new points all within ftol of
c a minimum function value, and iter gives the number of function evaluations taken.

      INTEGER i,ihi,ilo,inhi,j,m,n
      REAL*8 rtol,sum,swap,ysave,ytry,psum(NDIMMAX),amotry
      iter=0
1     do 12 n=1,ndim ! Enter here when starting or have just overall contracted.
        sum=0.d0 ! Recompute psum.
        do 11 m=1,ndim+1
          sum=sum+p(m,n)
11      enddo
        psum(n)=sum
12    enddo
2     ilo=1 ! Enter here when have just changed a single point.
      if (y(1).gt.y(2)) then ! Determine which point is the highest (worst), next-highest,
        ihi=1                ! and lowest (best),
        inhi=2
      else
        ihi=2
        inhi=1
      endif
      do 13 i=1,ndim+1 ! by looping over the points in the simplex.
        if(y(i).le.y(ilo)) ilo=i
        if(y(i).gt.y(ihi)) then
          inhi=ihi
          ihi=i
        else if(y(i).gt.y(inhi)) then
          if(i.ne.ihi) inhi=i
        endif
13    enddo
      rtol=2.d0*abs(y(ihi)-y(ilo))/(abs(y(ihi))+abs(y(ilo))+TINY)
c Compute the fractional range from highest to lowest and return if satisfactory.
      if (rtol.lt.ftol) then ! If returning, put best point and value in slot 1.
        swap=y(1)
        y(1)=y(ilo)
        y(ilo)=swap
        do 14 n=1,ndim
          swap=p(1,n)
          p(1,n)=p(ilo,n)
          p(ilo,n)=swap
14      enddo
        return
      endif
      if (iter.ge.itmax) then
        write(*,*) '# amoeba(): Warning - itmax exceeded in amoeba'
        return
      endif
      iter=iter+2
c Begin a new iteration. First extrapolate by a factor −1 through the face of the simplex across
c from the high point, i.e., reflect the simplex from the high point.
      ytry=amotry(p,y,psum,mp,np,ndim,funk,ihi,-1.0d0)
      if (ytry.le.y(ilo)) then
c Gives a result better than the best point, so try an additional extrapolation by a factor 2.
        ytry=amotry(p,y,psum,mp,np,ndim,funk,ihi,2.0d0)
      else if (ytry.ge.y(inhi)) then
c The reflected point is worse than the second-highest, so look for an intermediate lower point,
c i.e., do a one-dimensional contraction.
        ysave=y(ihi)
        ytry=amotry(p,y,psum,mp,np,ndim,funk,ihi,0.5d0)
        if (ytry.ge.ysave) then ! Can’t seem to get rid of that high point. Better contract
          do 16 i=1,ndim+1 ! around the lowest (best) point.
            if(i.ne.ilo)then
              do 15 j=1,ndim
                psum(j)=0.5d0*(p(i,j)+p(ilo,j))
                p(i,j)=psum(j)
15            enddo
              y(i)=funk(psum)
            endif
16        enddo
          iter=iter+ndim ! Keep track of function evaluations.
          goto 1 ! Go back for the test of doneness and the next iteration.
        endif
      else
        iter=iter-1 ! Correct the evaluation count.
      endif
      goto 2
      END


