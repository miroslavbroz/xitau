      SUBROUTINE qtrap(func,a,b,eps,s)
      implicit none
      INTEGER JMAX
      REAL*8 a,b,func,s,eps
      EXTERNAL func
      PARAMETER (JMAX=20)
C     USES trapzd
c          Returns as s the integral of the function func from a to b. The parameters eps can be set
c          to the desired fractional accuracy and JMAX so that 2 to the power JMAX-1 is the maximum
c          allowed number of steps. Integration is performed by the trapezoidal rule.
      INTEGER j
      REAL*8 olds
      olds=-1.d30  ! Any number that is unlikely to be the average of the function

      do j=1,JMAX                  ! at its endpoints will do here.
        call trapzd(func,a,b,s,j)
        if (j.gt.5) then           ! Avoid spurious early convergence.
          if (abs(s-olds).lt.eps*abs(olds).or.
     :      (s.eq.0.d0.and.olds.eq.0.d0)) return
          endif
        olds=s
      enddo

      write(*,*) "qtrap: s = ", s
      write(*,*) "qtrap: olds = ", olds
      write(*,*) "qtrap: eps = ", eps
      write(*,*) "qtrap: abs(s-olds) = ", abs(s-olds)
      write(*,*) "qtrap: eps*abs(olds) = ", eps*abs(olds)
      write(*,*) "Error: Too many steps in qtrap!"
      stop
      END

