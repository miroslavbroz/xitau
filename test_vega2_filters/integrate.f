c integrate.f
c Integrate an array bysimple trapezoidal rule.
c Miroslav Broz (miroslav.broz@email.cz), Jun 22nd 2016

      real*8 function integrate(n, x, y, x1, x2)
      implicit none
c input
      integer n
      real*8 x(n), y(n), x1, x2
c internal
      integer i
      real*8 sum, y1, y2
c functions
      real*8 interp

      if ((x1.lt.x(1)).or.(x2.gt.x(n))) then
        write(*,*) 'integrate.f: Error extrapolation not allowed!'
        stop
      endif

      sum = 0.d0
      i = 2
      do while ((x(i).lt.x1).and.(i.lt.n))
        i = i+1
      enddo

      y1 = interp(x(i-1), x(i), y(i-1), y(i), x1)
      sum = sum + 0.5d0*(x(i)-x1)*(y(i)+y1)
c      write(*,*) "x(i-1) = ", x(i-1)
c      write(*,*) "x(i) = ", x(i)
c      write(*,*) "x1 = ", x1
c      write(*,*) "y1 = ", y1
c      write(*,*) "sum = ", sum
c      stop

      i = i+1
      do while ((x(i).lt.x2).and.(i.lt.n))
        sum = sum + 0.5d0*(x(i)-x(i-1))*(y(i)+y(i-1))
        i = i+1
      enddo

      y2 = interp(x(i-1), x(i), y(i-1), y(i), x2)
      sum = sum + 0.5d0*(x2-x(i-1))*(y2+y(i-1))

      integrate = sum
      return
      end


