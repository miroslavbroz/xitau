c read_ephemeris.f
c Read ephemeris.
c Miroslav Broz (miroslav.broz@email.cz), Aug 24th 2020

      subroutine read_ephemeris(filename,N,t,vardist,l,b)

      implicit none
      include '../misc/const.inc'
      include 'simplex.inc'

      character*(*) filename
      integer N
      real*8 t(OBSMAX),vardist(OBSMAX),l(OBSMAX),b(OBSMAX)

      integer i,length,ierr
      character*255 str

      if (filename(1:1).eq.'-') then
        N = 0
        return
      endif

      i = 0
      open(unit=10,file=filename,status="old",form="formatted",
     :  iostat=ierr)
      if (ierr.ne.0) then
        write(*,*) "read_ephemeris.f: Error opening file '",
     :     trim(filename),"'."
        stop
      endif

5     continue
        read(10,10,err=20,end=20) str
10      format(a)
        if ((str(1:1).ne.'#').and.(length(str).gt.0)) then
          i = i+1
          if (i.le.OBSMAX) then
            read(str,*,err=20,end=20) t(i),vardist(i),l(i),b(i)
          else
            write(*,*) "read_ephemeris.f: Error number of data .gt. ",
     :        "OBSMAX = ", OBSMAX
            stop
          endif

          l(i) = l(i)*deg
          b(i) = b(i)*deg
        endif
      goto 5
20    continue
      close(10)

      N = i

      return
      end
