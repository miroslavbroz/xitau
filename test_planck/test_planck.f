      program test_planck

      implicit none
      integer nbod
      parameter(nbod=3)
      integer i, k, n
      real*8 T(nbod), Lum(nbod)
      real*8 lambda, lambda1, lambda2
      real*8 planck

      T(1) = 10700.d0
      T(2) = 10480.d0
      T(3) = 14190.d0
      lambda1 = 0.d0
      lambda2 = 2500.d-9
      n = 250
      do i = 0, n
        lambda = lambda1 + (lambda2-lambda1)*(1.d0*i/n)
        do k = 1,nbod
          Lum(k) = planck(T(k), lambda)
        enddo
        write(*,*) lambda, (Lum(k), k=1,nbod)
      enddo

      return
      end

