c
c Angular momentum L = Sum_i^n r_i x m_i v_i.
c
      subroutine angmom(n, m, y, L)

      include 'common.inc'

      integer n
      real*8 m(NPLMAX), y(6,NPLMAX), L(3)

      integer i, k
      real*8 c(3)

      do k = 1, 3
        L(k) = 0.d0
      enddo
      do i = 1, n
        call vproduct(y(1,i), y(4,i), c)
        do k = 1, 3
          L(k) = L(k) + m(i)*c(k)
        enddo
      enddo

      return
      end

