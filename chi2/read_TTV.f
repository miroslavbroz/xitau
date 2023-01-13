c read_TTV.f
c Read transit-timing varations data.
c Miroslav Broz (miroslav.broz@email.cz), Nov 14th 2009

      subroutine read_TTV(filename,N,t,sigma_t)

      implicit none
      include 'chi2.inc'

      character*(*) filename
      integer N
      real*8 t(OBSMAX),sigma_t(OBSMAX)
      real*8 epoch,omc
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
        write(*,*) "read_CLO.f: Error opening file '",
     :     trim(filename),"'."
        stop
      endif

5     continue
        read(10,10,err=20,end=20) str
10      format(a)
        if ((str(1:1).ne."#").and.(length(str).gt.0)) then
          i = i+1
          if (i.le.OBSMAX) then
            read(str,*,err=20,end=20) t(i),epoch,omc,sigma_t(i)
          else
            write(*,*) "read_TTV.f: Error number of observations .gt. ",
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

