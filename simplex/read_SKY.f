c read_SKY.f
c Read speckle-interferometry data.
c Miroslav Broz (miroslav.broz@email.cz), Mar 1st 2016

      subroutine read_SKY(filename,N,t,xh,yh,major,minor,PA_ellipse,
     :  vardist,l,b)

      implicit none
      include '../misc/const.inc'
      include 'simplex.inc'
      include 'dependent.inc'

      character*(*) filename
      integer N
      real*8 t(OBSMAX),xh(OBSMAX),yh(OBSMAX)
      real*8 major(OBSMAX),minor(OBSMAX),PA_ellipse(OBSMAX)
      real*8 vardist(OBSMAX),l(OBSMAX),b(OBSMAX)

      real*8 rho,theta
      real*8 t_(OBSMAX)
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
        write(*,*) "read_SKY.f: Error opening file '",
     :     trim(filename),"'."
        stop
      endif

5     continue
        read(10,10,err=20,end=20) str
10      format(a)
        if ((str(1:1).ne.'#').and.(length(str).gt.0)) then
          i = i+1
          if (i.le.OBSMAX) then
            read(str,*,err=20,end=20) t(i),rho,theta,
     :        major(i),minor(i),PA_ellipse(i)
          else
            write(*,*) "read_SKY.f: Error number of observations .gt. ",
     :        "OBSMAX = ", OBSMAX
            stop
          endif

          theta = theta*deg
          PA_ellipse(i) = PA_ellipse(i)*deg
          xh(i) = -rho*sin(theta)
          yh(i) = rho*cos(theta)
        endif
      goto 5
20    continue
      close(10)

      N = i

      if (use_vardist) then
        str = filename(1:length(filename)-4) // '.eph'
        call read_ephemeris(str,i,t_,vardist,l,b)
      endif
      if (i.ne.N) then
        write(*,*) "read_SKY.f: Error number of ephmerides .ne. ", N
        stop
      endif

      return
      end

