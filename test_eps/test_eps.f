
      program test_eps
      implicit none
      include '../misc/const.inc'
      integer i, n
      real*8 jd, eps, dt
      real*8 eps_earth

      n = 1000
      dt = 20000.d0*365.25d0/n

      do i = 0, n
        jd = 2451545.d0 - 10000.d0*365.25d0 + i*dt
        eps = eps_earth(jd)
        write(*,*) jd, eps/deg
      enddo

      stop
      end

