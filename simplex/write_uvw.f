c write_uvw.f
c Write (u, v, w) coordinates.
c Miroslav Broz (miroslav.broz@email.cz), Aug 24th 2020

      subroutine write_uvw(NOUT, tout, rh, rp, rp3)

      implicit none

      include '../misc/const.inc'
      include 'simplex.inc'
      include 'dependent.inc'

      integer NOUT
      real*8 tout(OUTMAX)
      real*8 rh(OUTMAX,NBODMAX,3),rp(OUTMAX,NBODMAX,3),
     :  rp3(OUTMAX,NBODMAX,3)

      integer i, j, k, iu
      real*8 l, b, x, y, z, u, v, w, d
      real*8 hatu(3), hatv(3), hatw(3)

      integer N
      real*8 t(OUTMAX),vardist(OUTMAX),ecl(OUTMAX),ecb(OUTMAX)

c functions
      real*8 interp,interp2

      data iu /10/
c
c read ephemeris
c
      call read_ephemeris("ephemeris_E.dat", N, t, vardist, ecl, ecb)
c
c write u, v, w
c
      open(unit=iu, file="out_JDATE_uvw.dat", status="unknown")
      write(iu,*) "# JD & ibod & u [au] & v [au] & w [au] & d [au]"

      do i = 1, NOUT

        j = 2
        do while ((j.lt.N).and.(t(j).le.tout(i)))
          j = j+1
        enddo

        l = interp2(t(j-1), t(j), ecl(j-1), ecl(j), tout(i))
        b = interp(t(j-1), t(j), ecb(j-1), ecb(j), tout(i))
        d = interp(t(j-1), t(j), vardist(j-1), vardist(j), tout(i))

        call uvw1(tout(i), l, b, hatu, hatv, hatw)

        do k = 1, nbod
          x = rh(i,k,1)
          y = rh(i,k,2)
          z = rh(i,k,3)

          call uvw2(hatu, hatv, hatw, x, y, z, u, v, w)

          write(iu,*) tout(i), -k, u, v, w, d
        enddo
      enddo

      close(iu)

      return
      end


