c uvw.f
c Compute sky-plane coordinates from l, b.
c Miroslav Broz (miroslav.broz@email.cz), Nov 21st 2019

c Reference: Broz & Wolf (2017), Astronomicka mereni, p. 168

      subroutine uvw(t, l, b, x, y, z, u, v, w)

      implicit none

      real*8 t, l, b, x, y, z, u, v, w
      real*8 hatu(3), hatv(3), hatw(3)

      call uvw1(t, l, b, hatu, hatv, hatw)
      call uvw2(hatu, hatv, hatw, x, y, z, u, v, w)

      return
      end


