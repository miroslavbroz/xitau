c geometries.f
c An interface for various geometries.
c Miroslav Broz (miroslav.broz@email.cz), Jan 19th 2025

      subroutine geometries(nbod, m, r, v, elmts, geometry)

      implicit none
      include '../chi2/chi2.inc'
c input
      integer nbod
      real*8 m(NBODMAX)
      real*8 r(NBODMAX,3), v(NBODMAX,3)
      integer geometry
c output
      real*8 elmts(NBODMAX,6)

      if (geometry.eq.3) then

        call geometry_ecliptic(nbod, m, r, v, elmts)

      else if (geometry.eq.4) then

        call geometry_hierarch2(nbod, m, r, v, elmts)

      else
        write(*,*) "geometries.f: Error unknown geometry = ",
     :    geometry
        stop
      endif

      return
      end

