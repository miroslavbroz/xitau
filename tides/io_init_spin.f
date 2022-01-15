c**********************************************************************
c IO_INIT_SPIN.F
c**********************************************************************
c Read in spin axes data.
c
c Input:
c  infile	name of the spin axes data input file
c  nbod		number of massive bodies
c
c  in common block /spin/
c  s(3,NTPMAX)		orientation of the spin axis	
c  omega(NTPMAX)	spin rates
c
c Remarks: 
c Author:  Miroslav Broz, miroslav.broz@email.cz
c Date:  Jan 19th 2009

      subroutine io_init_spin(infile,nbod)

      include '../swift.inc'
      include 'spin.inc'

c  input
      character*(*) infile
      integer nbod

c  internal
      integer i,ierr,nbodchk

c  main     
      write(*,*) '# Spin axes data file called ',trim(infile)
      call io_open(7,infile,'old','formatted',ierr)

      read(7,*,err=99,end=99) nbodchk
      if(nbod.eq.0) nbod = nbodchk
      if(nbodchk.ne.nbod) then
        write(*,*) 'in io_init_spin: Error number of bodies nbodchk = ',
     :     nbodchk, ' differs from nbod = ', nbod, ' in file ', infile
        call util_exit(1)
      endif

      do i=1,nbod
        read(7,*,err=99,end=99) s(1,i),s(2,i),s(3,i),omega(i)
      enddo

      close(unit = 7)
      write(*,*) ' '
      return

99    continue
      write(*,*) 'Error reading file ',infile
      call util_exit(1)

      end    ! io_init_spin.f
c-----------------------------------------------------------------


