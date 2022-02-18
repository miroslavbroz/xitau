c omega_kopal_approx.f
c An approximate value of Kopal potential computed from given (mean) radius.
c Miroslav Broz (miroslav.broz@email.cz), Jun 1st 2016

      real*8 function omega_kopal_approx(R, q, x0)
      implicit none
      include '../misc/const.inc'
c input
      real*8 R, q, x0
c internal
      real*8 phi, theta, x, y, z, r1, r2, r3, Omega_, sum, tmp
      integer i, j, n, m
c functions
      real*8 omega_kopal

!      open(unit=25, file='omega_kopal.dat', status='unknown')
!      write(25,*) '# x [a=1] & y & z & Omega_kopal [a^-1]'

      n = 32
      m = 32
      sum = 0.d0
      do i = 1, n
        do j = 1, m
          phi = 2.d0*pi_*(i-1)/n
          theta = -0.5d0*pi_ + pi_*(j-1)/n
          x = R*cos(phi)*cos(theta) + x0
          y = R*sin(phi)*cos(theta)
          z = R*sin(theta)
          r1 = sqrt(x**2 + y**2 + z**2)
          r2 = sqrt((x-1.d0)**2 + y**2 + z**2)
          r3 = sqrt((x-q/(1.d0+q))**2 + y**2)
          tmp = omega_kopal(r1, r2, r3, q)
          sum = sum + tmp
!          write(25,*) x, y, z, tmp  ! dbg
        enddo
      enddo

!      close(25)

      omega_kopal_approx = sum/(n*m)
      return
      end

