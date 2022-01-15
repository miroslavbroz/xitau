c test_zeta.f
c Test "derotator" angle computation.
c Miroslav Broz (miroslav.broz@email.cz), Aug 31st 2020

      program test_zeta

      implicit none
      include '../misc/const.inc'

      integer i, j
      real*8 l, b, eps, zeta

      eps = eps_J2000

      do i = 0, 360
        do j = -90, 90
          l = dble(i)*deg
          b = dble(j)*deg

          zeta = atan2(sin(eps)*cos(l),
     :      (cos(b)*cos(eps) - sin(b)*sin(eps)*sin(l)))

          write(*,*) l/deg, b/deg, zeta/deg
        enddo
        write(*,*)
      enddo

      stop
      end

