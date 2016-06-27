c geometries.f
c An interface for various geometries.
c Miroslav Broz (miroslav.broz@email.cz), Jun 14th 2016

      subroutine geometries(nbod, m, elmts, r, v, geometry)

      include 'simplex.inc'
c input
      integer nbod
      real*8 m(NBODMAX)
      real*8 elmts(NBODMAX,6)
      integer geometry
c output
      real*8 r(NBODMAX,3), v(NBODMAX,3)

      if (geometry.eq.0) then

        call geometry_hierarch(nbod, m, elmts, r, v)

      else if (geometry.eq.1) then

        call geometry_twopairs(nbod, m, elmts, r, v)

      else
        write(*,*) "geometries.f: Error unknown geometry = ",
     :    geometry
        stop
      endif

      return
      end

