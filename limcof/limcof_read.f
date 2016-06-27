c limcof_read.f
c Read limb-darkening coefficient tables.
c Miroslav Broz (miroslav.broz@email.cz), Jun 2nd 2016

      subroutine limcof_read(filename)

      implicit none
      include 'limcof.inc'
c input
      character*(*) filename
c internal
      integer i, j, iu, ierr
      character*80 str
      data iu /10/

c read file
      open(unit=iu, file=filename, status="old", iostat=ierr)
      if (ierr.ne.0) then
        write(*,*) "limcof_read.f: Error opening file '",
     :    trim(filename), "'."
        stop
      endif

      i = 0
      do while (ierr.eq.0)
        read(iu, "(a)", iostat=ierr) str
        if ((ierr.eq.0).and.(str(1:1).ne.'#')) then
          i = i+1
          read(str, *, iostat=ierr) lambda_limb(i), Teff_limb(i),
     :      logg_limb(i), Z_limb(i), u_limb(i)
          if (ierr.ne.0) then
            i = i-1
          endif
        endif
      enddo
      n_limb = i

      close(iu)

c prepare avalability data
      j = 0
      do i = 1, n_limb
        if ((j.eq.0)
     :    .or.(Teff_limb(i).ne.Teff_avail(j))
     :    .or.(logg_limb(i).ne.logg_avail(j))
     :    .or.(Z_limb(i).ne.Z_avail(j))) then
          j = j+1
          if (j.gt.AVAILMAX) then
            write(*,*) "limcof_read.f: Error number of available data",
     :        " n_avail = ", j, " .gt. AVAILMAX = ", AVAILMAX
            stop
          endif
          Teff_avail(j) = Teff_limb(i)
          logg_avail(j) = logg_limb(i)
          Z_avail(j) = Z_limb(i)
        endif
      enddo
      n_avail = j

      return
      end


