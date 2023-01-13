c io_write_spin.f
c Write spin axis orientations to output file.
c Miroslav Broz (miroslav.broz@email.cz), Jan 19th 2010

      subroutine io_write_spin(time, nbod_, outfile, iu, fopenstat)

      include '../swift.inc'
      include 'spin.inc'
      include '../chi2/chi2.inc'
      include '../chi2/dependent.inc'

      real*8 time
      integer nbod_, iu
      character*(*) outfile, fopenstat

c internal
      integer i,ierr,i1st
      real*8 time_,sgn
c      data i1st /0/
c      save i1st

      if (is_forward) then
        time_ = T0+time
        sgn = 1.d0
      else
        time_ = T0-time
        sgn = -1.d0
      endif

      call io_open(iu,outfile,fopenstat,'formatted',ierr)
c      if (i1st.eq.0) then
c        call io_open(iu,outfile,fopenstat,'formatted',ierr)
c        i1st = 1
c      else
c        call io_open(iu,outfile,'append','formatted',ierr)
c      endif
      if (ierr.ne.0) then
        write(*,*) 'io_write_spin: Error opening file ', outfile
        call util_exit(1)
      endif

      do i = 1, nbod_
        write(iu,*) -i, time_, sgn*s(1,i),sgn*s(2,i),sgn*s(3,i),
     :    omega(i)
      enddo

      close(unit=iu)

      return
      end	! io_write_spin
c-----------------------------------------------------------------

