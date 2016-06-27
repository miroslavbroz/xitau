      SUBROUTINE amebsa(p,y,mp,np,ndim,pb,yb,ftol,funk,iter,temptr)
      implicit none
      INTEGER iter,mp,ndim,np,NMAX
      REAL*8 ftol,temptr,yb,p(mp,np),pb(np),y(mp),funk
      PARAMETER (NMAX=200)
      EXTERNAL funk
C USES amotsa,funk,ran1
c      Multidimensional minimization of the function funk(x) where x(1:ndim) is a vector in
c      ndim dimensions, by simulated annealing combined with the downhill simplex method of
c      Nelder and Mead. The input matrix p(1..ndim+1,1..ndim) has ndim+1 rows, each an
c      ndim-dimensional vector which is a vertex of the starting simplex. Also input is the vector
c      y(1:ndim+1) , whose components must be pre-initialized to the values of funk evaluated at
c      the ndim+1 vertices (rows) of p; ftol, the fractional convergence tolerance to be achieved
c      in the function value for an early return; iter, and temptr. The routine makes iter
c      function evaluations at an annealing temperature temptr, then returns. You should then
c      decrease temptr according to your annealing schedule, reset iter, and call the routine
c      again (leaving other arguments unaltered between calls). If iter is returned with a positive
c      value, then early convergence and return occurred. If you initialize yb to a very large value
c      on the first call, then yb and pb(1:ndim) will subsequently return the best function value
c      and point ever encountered (even if it is no longer a point in the simplex).
      INTEGER i,idum,ihi,ilo,j,m,n
      REAL*8 rtol,sum,swap,tt,yhi,ylo,ynhi,ysave,yt,ytry,psum(NMAX),
     :  amotsa,ran1
      COMMON /ambsa/ tt,idum
      tt=-temptr
1     do n=1,ndim                    ! Enter here when starting or after overall contraction.
        sum=0.                          ! Recompute psum.
        do m=1,ndim+1
          sum=sum+p(m,n)
        enddo
        psum(n)=sum
      enddo
2     ilo=1                               ! Enter here after changing a single point. Find which point
      ihi=2                               !   is the highest (worst), next-highest, and lowest (best).
      ylo=y(1)+tt*log(ran1(idum))         ! Whenever we "look at" a vertex, it gets a random thermal
      ynhi=ylo                            !   fluctuation.
      yhi=y(2)+tt*log(ran1(idum))
      if (ylo.gt.yhi) then
        ihi=1
        ilo=2
        ynhi=yhi
        yhi=ylo
        ylo=ynhi
      endif
      do i=3,ndim+1                     ! Loop over the points in the simplex.
        yt=y(i)+tt*log(ran1(idum))      ! More thermal fluctuations.
        if (yt.le.ylo) then
          ilo=i
          ylo=yt
        endif
        if (yt.gt.yhi) then
          ynhi=yhi
          ihi=i
          yhi=yt
        else if(yt.gt.ynhi) then
          ynhi=yt
        endif
      enddo
      rtol=2.*abs(yhi-ylo)/(abs(yhi)+abs(ylo))
c      Compute the fractional range from highest to lowest and return if satisfactory.
      if (rtol.lt.ftol.or.iter.lt.0) then  ! If returning, put best point and value in slot 1.
        swap=y(1)
        y(1)=y(ilo)
        y(ilo)=swap
        do n=1,ndim
          swap=p(1,n)
          p(1,n)=p(ilo,n)
          p(ilo,n)=swap
        enddo
        return
      endif
      iter=iter-2
c      Begin a new iteration. First extrapolate by a factor -1 through the face of the simplex across
c      from the high point, i.e., reflect the simplex from the high point.
      ytry=amotsa(p,y,psum,mp,np,ndim,pb,yb,funk,ihi,yhi,-1.0d0)
      if (ytry.le.ylo) then
c        Gives a result better than the best point, so try an additional extrapolation by a factor 2.
        ytry=amotsa(p,y,psum,mp,np,ndim,pb,yb,funk,ihi,yhi,2.0d0)
      else if (ytry.ge.ynhi) then
c        The reflected point is worse than the second-highest, so look for an intermediate lower point,
c        i.e., do a one-dimensional contraction.
        ysave=yhi
        ytry=amotsa(p,y,psum,mp,np,ndim,pb,yb,funk,ihi,yhi,0.5d0)
        if (ytry.ge.ysave) then          ! Can't seem to get rid of that high point. Better contract
          do i=1,ndim+1            ! around the lowest (best) point.
            if (i.ne.ilo) then
              do j=1,ndim
                psum(j)=0.5*(p(i,j)+p(ilo,j))
                p(i,j)=psum(j)
              enddo
              y(i)=funk(psum)
            endif
          enddo
          iter=iter-ndim
          goto 1
        endif
      else
        iter=iter+1                      ! Correct the evaluation count.
      endif
      goto 2
      END


