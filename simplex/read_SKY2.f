c read_SKY2.f
c Read differential astrometry data.
c Miroslav Broz (miroslav.broz@email.cz), Sep 7th 2020

      subroutine read_SKY2(filename,N,t,one,two,x12,y12,major,minor,
     :  PA_ellipse,vardist,l,b)

      implicit none
      include '../misc/const.inc'
      include 'simplex.inc'

      character*(*) filename
      integer N
      integer one(OBSMAX), two(OBSMAX)
      real*8 t(OBSMAX),x12(OBSMAX),y12(OBSMAX)
      real*8 major(OBSMAX),minor(OBSMAX),PA_ellipse(OBSMAX)
      real*8 vardist(OBSMAX),l(OBSMAX),b(OBSMAX)

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
        write(*,*) "read_SKY2.f: Error opening file '",
     :     trim(filename),"'."
        stop
      endif

5     continue
        read(10,10,err=20,end=20) str
10      format(a)
        if ((str(1:1).ne.'#').and.(length(str).gt.0)) then
          i = i+1
          if (i.le.OBSMAX) then
            read(str,*,err=20,end=20) t(i),one(i),two(i),x12(i),y12(i),
     :        major(i),minor(i),PA_ellipse(i),vardist(i),l(i),b(i)
          else
            write(*,*) "read_SKY2.f: Error number of observations",
     :        " .gt. OBSMAX = ", OBSMAX
            stop
          endif

          PA_ellipse(i) = PA_ellipse(i)*deg
          l(i) = l(i)*deg
          b(i) = b(i)*deg
        endif
      goto 5
20    continue
      close(10)

      N = i

      return
      end
