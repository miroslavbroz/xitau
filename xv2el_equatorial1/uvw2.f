c uvw2.f
c Compute sky-plane coordinates from hatu, hatv, hatw.
c Miroslav Broz (miroslav.broz@email.cz), Nov 21st 2019

c Reference: Broz & Wolf (2017), Astronomicka mereni, p. 168

      subroutine uvw2(hatu, hatv, hatw, x, y, z, u, v, w)

      implicit none

      real*8 hatu(3), hatv(3), hatw(3)
      real*8 x, y, z, u, v, w

      u = hatu(1)*x + hatu(2)*y + hatu(3)*z
      v = hatv(1)*x + hatv(2)*y + hatv(3)*z
      w = hatw(1)*x + hatw(2)*y + hatw(3)*z

      return
      end


