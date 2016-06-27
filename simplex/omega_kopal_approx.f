c omega_kopal_approx.f
c An approximate value of Kopal potential computed from given (mean) radius.
c Miroslav Broz (miroslav.broz@email.cz), Jun 1st 2016

      real*8 function omega_kopal_approx(R, q, x0)
      implicit none
      include '../misc/const.inc'
c input
      real*8 R, q, x0
c internal
      real*8 phi, x, y, z, r1, r2, r3, Omega_, sum
      integer i, n
c functions
      real*8 omega_kopal

      n = 32
      sum = 0.d0
      do i = 1, n
        phi = 2.d0*pi_*(i-1)/n
        x = R*cos(phi) + x0
        y = R*sin(phi)
        z = 0.d0
        r1 = sqrt(x**2 + y**2 + z**2)
        r2 = sqrt((x-1.d0)**2 + y**2 + z**2)
        r3 = sqrt((x-q/(1.d0+q))**2 + y**2)
        sum = sum + omega_kopal(r1, r2, r3, q)
      enddo

      omega_kopal_approx = sum/n
      return
      end

