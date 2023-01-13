c read_ECL.f
c Read eclipse durations data.
c Miroslav Broz (miroslav.broz@email.cz), Nov 14th 2009

      subroutine read_ECL(filename,N,t,d,sigma_d)

      implicit none
      include 'chi2.inc'

      character*(*) filename
      integer N
      real*8 t(OBSMAX),d(OBSMAX),sigma_d(OBSMAX)
      integer i,length,ierr
      character*80 str

      if (filename(1:1).eq.'-') then
        N = 0
        return
      endif

      i = 0
      open(unit=10,file=filename,status="old",form="formatted",
     :  iostat=ierr)
      if (ierr.ne.0) then
        write(*,*) "read_ECL.f: Error opening file '",
     :     trim(filename),"'."
        stop
      endif

5     continue
        read(10,10,err=20,end=20) str
10      format(a)
        if ((str(1:1).ne."#").and.(length(str).gt.0)) then
          i = i+1
          if (i.le.OBSMAX) then
            read(str,*,err=20,end=20) t(i),d(i),sigma_d(i)
          else
            write(*,*) "read_ECL.f: Error number of observations .gt. ",
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

