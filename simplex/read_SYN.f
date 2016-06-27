c read_SYN.f
c Read observed spectra (to compare with synthetic ones).
c Miroslav Broz (miroslav.broz@email.cz), May 23rd 2016

      subroutine read_SYN(filename, N, t, lambda, Int_lambda,
     :  sigma_Int, dataset)

      implicit none
      include 'simplex.inc'

      character*(*) filename
      integer N
      real*8 t(OBSMAX), lambda(OBSMAX), Int_lambda(OBSMAX),
     :  sigma_Int(OBSMAX)
      integer dataset(OBSMAX)
      integer i,length,ierr
      character*80 str

      if (filename(1:1).eq.'-') then
        N = 0
        return
      endif

      open(unit=10,file=filename,status="old",form="formatted",
     :  iostat=ierr)
      if (ierr.ne.0) then
        write(*,*) "read_SYN.f: Error opening file '", filename, "'."
        stop
      endif

      i = 0
5     continue
        read(10,10,err=20,end=20) str
10      format(a)
        if ((str(1:1).ne."#").and.(length(str).gt.0)) then
          i = i+1
          if (i.le.OBSMAX) then
            read(str,*,err=20,end=20) t(i), lambda(i), Int_lambda(i),
     :        sigma_Int(i), dataset(i)
          else
            write(*,*) "read_SYN.f: Error number of observations",
     :        " .gt. OBSMAX = ", OBSMAX
            stop
          endif
         endif
      goto 5
20    continue
      close(10)

      N = i
      return
      end

