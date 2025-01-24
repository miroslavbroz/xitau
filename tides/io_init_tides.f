c io_init_tides.f
c Read tides parameters file.
c Miroslav Broz (miroslav.broz@email.cz), Nov 7th 2013

      subroutine io_init_tides(infile,nbod)

      include '../swift.inc'
      include '../misc/const.inc'
      include 'tides.inc'
      include 'spin.inc'

c input
      character*(*) infile
      integer nbod

c output in /spin/ common block

c temporary
      integer i,j
      integer npl,ierr

c  read input parameters
      write(*,*) '# Tides parameters file called ',trim(infile)
      call io_open(7,infile,'old','formatted',ierr)
      if (ierr.ne.0) then
        write(*,*) 'io_init_tides: Error opening file ',infile,'.'
        call util_exit(1)
      endif

      read(7,*) npl

      if (npl.ne.nbod) then
        write(*,*) 'io_init_tides: Error number of planets npl = ',
     :    npl, ' .ne. nbod = ', nbod,'.'
        call util_exit(1)
      endif

      if (npl.gt.NPLMAX) then
        write(*,*) 'io_init_tides: Error number of planets npl = ',
     :     npl, ' .gt. NPLMAX = ', NPLMAX,'.'
        call util_exit(1)
      endif

      do i = 1, npl
        read(7,*) capR(i), k_2(i), Delta_t_(i), MoI(i)
      enddo

      read(7,*) dtspin
      read(7,*) dtspinout
      read(7,10) outspinfile
10    format(a)
      read(7,*) debug_spin
      
      close(unit = 7)
      write(*,*) ' '

c unit conversion
      do i = 1, npl
        capR(i) = capR(i) * 1.d3/AU  ! km -> AU
        Delta_t_(i) = Delta_t_(i)/day  ! sec -> day
        MoI(i) = MoI(i) * day**2/(AU**5)  ! kg m^2 -> AU^3/day^2 AU^2
      enddo

c compute angular momentum (assuming a PCA rotation)
      do i = 1, npl
        do j = 1, 3
          L_spin(j,i) = s(j,i) * MoI(i) * omega(i)
        enddo
      enddo

      write(*,*) "# Warning: Delta_t_ will be overwritten by Delta_t!"

      return
      end


