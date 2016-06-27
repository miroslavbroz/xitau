c io_init_tides.f
c Read tides.in input file.
c Miroslav Broz (miroslav.broz@email.cz), May 25th 2016

      subroutine io_init_tides(filename, nbod)

      include "../swift.inc"
      include "../misc/const.inc"
      include "tides.inc"

c input
      character*(*) filename
      integer nbod

c output is in /tides/ common block

c internal
      integer i, iu, ierr, nbod2
      real*8 P
      character*80 str
      data iu /10/

c functions
      integer length

      open(unit=iu, file=filename, status="old", iostat=ierr)
      if (ierr.ne.0) then
        write(*,*) "io_init_tides.f: Error opening file '",
     :    trim(filename), "'."
        stop
      endif

5     continue
        read(iu,10,err=990,end=990) str
10      format(a)
      if ((str(1:1).eq.'#').or.(length(str).eq.0)) goto 5

      read(str,*,err=990,end=990) nbod2
      if (nbod2.ne.nbod) then
        write(*,*) "io_init_tides.f: Error number of bodies nbod = ",
     :    nbod2, " .ne. ", nbod, "."
        stop
      endif

      do i = 1, nbod
        read(iu,*,err=990,end=990) k_L(i), P, R_body(i)

        Omega_rot(i) = 2.d0*pi/P  ! day -> rad/day
        R_body(i) = R_body(i)*R_S/AU  ! R_S -> AU

        write(*,*) "# k_L(", i, ") = ", k_L(i)
        write(*,*) "# Omega_rot(", i, ") = ", Omega_rot(i), " rad/day"
        write(*,*) "# R_body(", i, ") = ", R_body(i), " AU = ",
     :    R_body(i)*AU/R_S, " R_S"

c optimisation
        koef1(i) = 3.0d0*k_L(i)
        koef2(i) = 0.5d0*k_L(i)*Omega_rot(i)**2
        R_body5(i) = R_body(i)**5
      enddo

      read(iu,*,err=990,end=990) use_tides
      write(*,*) "# use_tides = ", use_tides

      read(iu,*,err=990,end=990) use_oblatness
      write(*,*) "# use_oblatness = ", use_oblatness

      read(iu,*,err=990,end=990) use_ppn
      write(*,*) "# use_ppn = ", use_ppn

      close(iu)

      return

990   continue
      write(*,*) "io_init_tides.f: Error reading file '",
     :  trim(filename), "'."
      stop

      end


