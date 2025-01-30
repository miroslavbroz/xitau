c
c  Transformation: cartesian -> spherical coordinates.
c  b(1) = r, b(2) = phi, b(3) = theta in usual notation.
c

      subroutine cartesian_spherical(a, b)

      implicit none
      real*8 a(3), b(3)

      b(1) = sqrt(a(1)**2 + a(2)**2 + a(3)**2)
      b(2) = datan2(a(2), a(1))
      b(3) = dasin(a(3)/b(1))

      return
      end

