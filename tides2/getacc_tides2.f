c getacc_tides2.f
c An external tidal acceleration (to be added).
c Miroslav Broz (miroslav.broz@email.cz), Sep 2nd 2020

c \vec a = Gm'/R^3 [ 3(\vec r\cdot\vec n)\vec n - \vec r]

      subroutine getacc_tides2(time,nbod_,mass,xb,yb,zb,axb,ayb,azb)

      include "../swift.inc"
      include "tides2.inc"
      include "../simplex/simplex.inc"
      include "../simplex/dependent.inc"

c input
      integer nbod_
      real*8 time
      real*8 mass(nbod_)
      real*8 xb(nbod_),yb(nbod_),zb(nbod_)

c output
      real*8 axb(nbod_),ayb(nbod_),azb(nbod_)

c internal
      integer i,j,i1st
      real*8 l,b,d,nx,ny,nz,time_
      real*8 x,y,z,acc,fac,ax,ay,az

      integer N
      real*8 t(OUTMAX),vardist(OUTMAX),ecl(OUTMAX),ecb(OUTMAX)

c functions
      real*8 interp

      save i1st, j, N, t, vardist, ecl, ecb
      data i1st /0/

      if (.not.use_tides2) return

      if (i1st.eq.0) then
        call read_ephemeris("ephemeris_S.dat", N, t, vardist, ecl, ecb)

        j = 2
        i1st = 1
      endif

      if (is_forward) then
        time_ = T0+time
      else
        time_ = T0-time
      endif

      do while ((j.gt.2).and.(t(j-1).gt.time_))
        j = j-1
      enddo
      do while ((j.lt.N).and.(t(j).le.time_))
        j = j+1
      enddo

      l = interp(t(j-1), t(j), ecl(j-1), ecl(j), time_)
      b = interp(t(j-1), t(j), ecb(j-1), ecb(j), time_)
      d = interp(t(j-1), t(j), vardist(j-1), vardist(j), time_)

      nx = -cos(l)*cos(b)
      ny = -sin(l)*cos(b)
      nz = -sin(b)

      do i = 2, nbod_
        x = xb(i) - xb(1)
        y = yb(i) - yb(1)
        z = zb(i) - zb(1)

        acc = external_mass/d**3
        fac = 3.d0*(x*nx + y*ny + z*nz)
        ax = acc*(fac*nx - x)
        ay = acc*(fac*ny - y)
        az = acc*(fac*nz - z)

        axb(i) = axb(i) + ax
        ayb(i) = ayb(i) + ay
        azb(i) = azb(i) + az
      enddo

      return
      end


