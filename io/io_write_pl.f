c io_write_pl.f
c Write out all massive particles.
c Miroslav Broz (miroslav.broz@email.cz), Mar 13th 2016

      subroutine io_write_pl(filename,t,nbod,mass,x,y,z,vx,vy,vz)

      character*(*) filename
      real*8 t
      integer nbod
      real*8 mass(nbod)
      real*8 x(nbod), y(nbod), z(nbod)
      real*8 vx(nbod), vy(nbod), vz(nbod)

      integer i, id, iu, ierr

      iu = 10
      open(unit=iu, file=filename, status="unknown", access="append",
     :  iostat=ierr)
      if (ierr.ne.0) then
        write(*,*) "io_write_pl: Error opening file '", filename, "'."
        stop
      endif

      do i = 1, nbod
        id = -i
        write(iu,20) t, id, x(i), y(i), z(i), vx(i), vy(i), vz(i)
20      format(e23.16,1x,i8,6(1x,e23.16))
      enddo

      close(iu)

      return
      end

