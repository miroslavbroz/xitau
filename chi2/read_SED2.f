c read_SED2.f
c Read spectral energy distribution data; individual component.
c Miroslav Broz (miroslav.broz@email.cz), Jan 21st 2025

      subroutine read_SED2(filename, N, lambda_eff, band_eff,
     :  mag, sigma, one, calibration, file_filter)

      implicit none
      include 'chi2.inc'

      integer STRMAX
      parameter(STRMAX = 16)

      character*(*) filename
      integer N
      integer one(SEDMAX)
      real*8 lambda_eff(SEDMAX), band_eff(SEDMAX), mag(SEDMAX),
     :  sigma(SEDMAX), calibration(SEDMAX)
      character*(*) file_filter(SEDMAX)

      integer i, j, length, ierr
      character*255 str, s(STRMAX)
c functions
      integer split

      if (filename(1:1).eq.'-') then
        N = 0
        return
      endif

      open(unit=10,file=filename,status="old",form="formatted",
     :  iostat=ierr)
      if (ierr.ne.0) then
        write(*,*) "read_SED2.f: Error opening file '",
     :     trim(filename),"'."
        stop
      endif

      i = 0
5     continue
        read(10,10,err=20,end=20) str
10      format(a)
        if ((str(1:1).ne."#").and.(length(trim(str)).gt.0)) then
          i = i+1
          if (i.le.SEDMAX) then
            read(str,*,err=20,end=20) lambda_eff(i), band_eff(i),
     :        mag(i), sigma(i), one(i), calibration(i)

c get also filename
            j = split(str, ' ', s, STRMAX)
            if (j.ge.7) then
              file_filter(i) = s(7)
            else
              write(*,*) "read_SED2.f: Errorneous data formattting: '",
     :          trim(str), "'; no TAB's, please!"
              stop
            endif

          else
            write(*,*) "read_SED2.f: Error number of observations",
     :        " .gt. SEDMAX = ", SEDMAX
            stop
          endif
        endif
      goto 5
20    continue
      close(10)

      N = i
      return
      end

