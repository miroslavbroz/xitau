c lsm.f
c LSM fit (y = a*x + b)
c Miroslav Broz (miroslav.broz@email.cz), Nov 14th 2009

      subroutine lsm(n,x,y,a,b,R)

      implicit none
      integer n
      real*8 x(n),y(n),a,b,R

      integer i
      real*8 Sx,Sxx,Sy,Syy,Sxy

      if (n.le.0) then
        a = 0.d0
        b = 0.d0
        R = 0.d0
        return
      endif

      Sx  = 0.d0
      Sxx = 0.d0
      Sy  = 0.d0
      Syy = 0.d0
      Sxy = 0.d0

      do i = 1,n
        Sx  = Sx  + x(i)
        Sxx = Sxx + x(i)*x(i)
        Sy  = Sy  + y(i)
        Syy = Syy + y(i)*y(i)
        Sxy = Sxy + x(i)*y(i)
      enddo
 
      a = (n*Sxy-Sx*Sy)/(n*Sxx-Sx*Sx)
      b = (Sy-a*Sx)/n
      R = (n*Sxy-Sx*Sy)/sqrt((n*Sxx-Sx*Sx)*(n*Syy-Sy*Sy))

c      a = Sy/Sx  ! for b = 0

      return
      end

