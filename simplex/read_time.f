c read_time.f
c Read time data only.
c Miroslav Broz (miroslav.broz@email.cz), Mar 18th 2016

c n = 0 has to be pre-initialised!
c if n > 0, append data to the array

      subroutine read_time(filename,n,t)

      implicit none
      include 'simplex.inc'

      character*80 filename
      integer n
      real*8 t(TIMEMAX)
      integer i,length,ierr
      character*80 str

      if (filename(1:1).eq.'-') then
        return
      endif

      open(unit=10,file=filename,status="old",form="formatted",
     :  iostat=ierr)
      if (ierr.ne.0) then
        write(*,*) "read_time.f: Error opening file '", filename, "'."
        stop
      endif

      i = n
5     continue
        read(10,10,err=20,end=20) str
10      format(a)
        if ((str(1:1).ne."#").and.(length(str).gt.0)) then
          i = i+1
          read(str,*,err=20,end=20) t(i)
          if ((i.gt.1).and.(t(i).eq.t(i-1))) then
            i = i-1
          endif
         endif
      goto 5
20    continue
      close(10)

      n = i
      return
      end

