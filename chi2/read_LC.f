c read_LC.f
c Read lightcurve data.
c Miroslav Broz (miroslav.broz@email.cz), Apr 19th 2016

      subroutine read_LC(filename, n, t, mag, sigma_mag)

      implicit none
      include 'chi2.inc'

c input
      character*(*) filename

c output
      integer n
      real*8 t(OBSMAX), mag(OBSMAX), sigma_mag(OBSMAX)

c temporary
      integer i, ierr
      character*255 str

c functions
      integer length

      if (filename(1:1).eq.'-') then
        n = 0
        return
      endif

      open(unit=10, file=filename, status="old", form="formatted",
     :  iostat=ierr)
      if (ierr.ne.0) then
        write(*,*) "read_LC.f: Error opening file '",
     :     trim(filename),"'."
        stop
      endif

      i = 0
      ierr = 0
      do while (ierr.eq.0)
        read(10, '(a)', iostat=ierr) str
        if (ierr.eq.0) then
          if ((str(1:1).ne."#").and.(length(str).gt.0)) then
            i = i + 1
            if (i.le.OBSMAX) then
              read(str, *, iostat=ierr) t(i), mag(i), sigma_mag(i)
            else
              write(*,*) "read_LC.f: Error number of observations",
     :          " .gt. OBSMAX = ", OBSMAX
            endif
          endif
        endif
      enddo

      close(10)

      n = i
      return
      end


