c filter.f
c Multiply array with another (interpolated) array.
c Miroslav Broz (miroslav.broz@email.cz), Jun 22nd 2016

      subroutine filter(n1, x1, y1, n2, x2, y2, n, x, y)

      implicit none
c input
      integer n1, n2
      real*8 x1(n1), y1(n1), x2(n2), y2(n2)
c output
      integer n
      real*8 x(n), y(n)
c internal
      integer i, j, k
      real*8 y1_interp, y2_interp
c functions
      real*8 interp

      i = 2
      do while ((x1(i).lt.x2(1)).and.(i.lt.n1))
        i = i+1
      enddo

c the 1st point should be zero anyway
c      y1_interp = interp(x1(i-1),x1(i),y1(i-1),y1(i),x2(1))
c      k = 1
c      x(k) = x2(1)
c      y(k) = y1_interp*y2(1)

      j = 2
      do while ((x1(i).lt.x2(n2)).and.(i.lt.n1))
        do while ((x2(j).lt.x1(i)).and.(j.lt.n2))
          j = j+1
        enddo

        y2_interp = interp(x2(j-1),x2(j),y2(j-1),y2(j),x1(i))

        k = k+1
        x(k) = x1(i)
        y(k) = y1(i)*y2_interp
        i = i+1
      enddo

c the end point should be also zero
c      k = k+1
c      x(k) = x2(n2)
c      y(k) = 0.d0

      n = k
      return
      end


