c limcof_test.f
c Quadrilinear interpolation of limb-darkening coefficients.
c Miroslav Broz (miroslav.broz@email.cz), Jun 6th 2016

      program limcof_test

      implicit none
      include 'limcof.inc'
c internal
      integer i, j, n, m
      real*8 Teff, logg, Z, lambda, u
      real*8 lambda1, lambda2
      real*8 lambda_(LAMBDAMAX), u_(LAMBDAMAX)
c functions
      real*8 interp

      Teff = 10400.d0
      logg = 4.25d0
      Z = -0.25d0
      write(*,*)
      write(*,*) "# Teff = ", Teff
      write(*,*) "# logg = ", logg
      write(*,*) "# Z = ", Z
      write(*,*)

      call limcof_read("limcof.dat")
      call limcof_interp(Teff, logg, Z, lambda_, u_, m)

      do j = 1, m
        write(*,*) lambda_(j), u_(j), j
      enddo
      write(*,*)

      lambda1 = 350.d-9
      lambda2 = 9200.d-9
      n = 1000
      j = 2
      do i = 0, n
        lambda = lambda1 + (lambda2-lambda1)*i/n
        do while ((lambda_(j).lt.lambda).and.(j.lt.m))
          j = j+1
        enddo
        u = interp(lambda_(j-1), lambda_(j), u_(j-1), u_(j), lambda)
        write(*,*) lambda, u, i, j
      enddo

      stop
      end


