c read_SED.f
c Read spectral energy distribution data.
c Miroslav Broz (miroslav.broz@email.cz), Jun 15th 2016

      subroutine read_SED(filename, N, lambda_eff, band_eff,
     :  mag, sigma, calibration, file_filter)

      implicit none
      include 'simplex.inc'

      integer STRMAX
      parameter(STRMAX = 16)

      character*(*) filename
      integer N
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
        write(*,*) "read_SED.f: Error opening file '",
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
     :        mag(i), sigma(i), calibration(i)

c get also filename
            j = split(str, ' ', s, STRMAX)
            if (j.ge.6) then
              file_filter(i) = s(6)
            else
              write(*,*) "read_SED.f: Errorneous data formattting: '",
     :          trim(str), "'; no TAB's, please!"
              stop
            endif

          else
            write(*,*) "read_SED.f: Error number of observations .gt. ",
     :        "SEDMAX = ", SEDMAX
            stop
          endif
        endif
      goto 5
20    continue
      close(10)

      N = i
      return
      end

