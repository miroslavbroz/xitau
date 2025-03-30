c read_synth.f
c Read synthetic spectra.
c Miroslav Broz (miroslav.broz@email.cz), May 23rd 2016

      subroutine read_synth(filename, N, lambda, Int_lambda)

      implicit none
      include 'chi2.inc'

      character*(*) filename
      integer N
      real*8 lambda(SYNMAX), Int_lambda(SYNMAX)
      integer i,length,ierr
      character*80 str

      if (filename(1:1).eq.'-') then
        N = 0
        return
      endif

      open(unit=10,file=filename,status="old",form="formatted",
     :  iostat=ierr)
      if (ierr.ne.0) then
        write(*,*) "read_synth.f: Error opening file '", trim(filename),
     :    "'."
        stop
      endif

      i = 0
5     continue
        read(10,10,err=20,end=20) str
10      format(a)
        if ((str(1:1).ne."#").and.(length(str).gt.0)) then
          i = i+1
          if (i.le.SYNMAX) then
            read(str,*,err=20,end=20) lambda(i), Int_lambda(i)

            lambda(i) = lambda(i)*1.d-10  ! A -> m
          else
            write(*,*) "read_synth.f: Error number of data points",
     :        " .gt. SYNMAX = ", SYNMAX
            stop
          endif
         endif
      goto 5
20    continue
      close(10)

      N = i
      return
      end

