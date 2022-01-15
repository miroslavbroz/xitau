c************************************************************************
c IO_DUMP_SPIN.F
c************************************************************************
c Dump spin axes orientations
c
c Input:
c  dspinfile		filename of dump_* file
c  nbod			number of PL's
c
c in common block /spin/
c  s(3,NTPMAX)		spin axes unit vectors
c  omega(i)		spin rates
c
c Author: Miroslav Broz, miroslav.broz@email.cz
c Date: Jan 19th 2010

      subroutine io_dump_spin(dspinfile,nbod)

      include '../swift.inc'
      include 'spin.inc'

      character*(*) dspinfile
      integer nbod

c  internal
      integer i,ierr

c  main ----------------------------------------------------------
      call io_open(7,dspinfile,'unknown','formatted',ierr)

      write(7,*) nbod

      do i=1,nbod
        write(7,*) s(1,i),s(2,i),s(3,i), omega(i)
      enddo

      close(unit=7)

      return
      end	! io_dump_spin
c-----------------------------------------------------------------

