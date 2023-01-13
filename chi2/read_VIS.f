c read_VIS.f
c Read interferometric visibility data.
c Miroslav Broz (miroslav.broz@email.cz), Apr 15th 2016

      subroutine read_VIS(filename, N, t, u, v, lambda_eff, band_eff,
     :  Vsq, sigma_Vsq, dataset)

      implicit none
      include 'chi2.inc'

      character*(*) filename
      integer N
      real*8 t(OBSMAX), u(OBSMAX), v(OBSMAX), lambda_eff(OBSMAX),
     :  band_eff(OBSMAX), Vsq(OBSMAX), sigma_Vsq(OBSMAX)
      integer dataset(OBSMAX)

      integer i,length,ierr
      character*255 str

      if (filename(1:1).eq.'-') then
        N = 0
        return
      endif

      open(unit=10,file=filename,status="old",form="formatted",
     :  iostat=ierr)
      if (ierr.ne.0) then
        write(*,*) "read_VIS.f: Error opening file '",
     :     trim(filename),"'."
        stop
      endif

      i = 0
5     continue
        read(10,10,err=20,end=20) str
10      format(a)
        if ((str(1:1).ne."#").and.(length(str).gt.0)) then
          i = i+1
          if (i.le.OBSMAX) then
            read(str,*,err=20,end=20) t(i), u(i), v(i), lambda_eff(i),
     :        band_eff(i), Vsq(i), sigma_Vsq(i), dataset(i)
          else
            write(*,*) "read_VIS.f: Error number of observations .gt. ",
     :        "OBSMAX = ", OBSMAX
            stop
          endif
        endif
      goto 5
20    continue
      close(10)

      N = i
      return
      end

