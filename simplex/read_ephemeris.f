c read_ephemeris.f
c Read ephemeris.
c Miroslav Broz (miroslav.broz@email.cz), Aug 24th 2020

      subroutine read_ephemeris(filename,N,t,vardist,l,b)

      use rotate_module

      implicit none
      include '../misc/const.inc'
      include 'simplex.inc'
      include 'dependent.inc'

      character*(*) filename
      integer N
      real*8 t(OBSMAX),vardist(OBSMAX),l(OBSMAX),b(OBSMAX)

      integer i,length,iu,ierr
      character*255 str
      real*8 eps,alpha,delta
      real*8 r(3),r_(3),tmp

c functions
      real*8 eps_earth

      data iu /10/

      if (filename(1:1).eq.'-') then
        N = 0
        return
      endif

      i = 0
      open(unit=iu,file=filename,status="old",form="formatted",
     :  iostat=ierr)
      if (ierr.ne.0) then
        write(*,*) "read_ephemeris.f: Error opening file '",
     :     trim(filename),"'."
        stop
      endif

5     continue
        read(iu,10,err=20,end=20) str
10      format(a)
        if ((str(1:1).ne.'#').and.(length(str).gt.0)) then
          i = i+1
          if (i.le.OBSMAX) then
            read(str,*,err=20,end=20) t(i),vardist(i),alpha,delta
          else
            write(*,*) "read_ephemeris.f: Error number of data .gt. ",
     :        "OBSMAX = ", OBSMAX
            stop
          endif

          alpha = alpha*deg
          delta = delta*deg

c equatorial J2000 -> ecliptic J2000
          eps = eps_earth(j2000)
          r(1) = cos(alpha)*cos(delta)
          r(2) = sin(alpha)*cos(delta)
          r(3) = sin(delta)
          r_ = rot_x(r, cos(-eps), sin(-eps))
          l(i) = atan2(r_(2),r_(1))
          b(i) = asin(r_(3))
        endif
      goto 5
20    continue
      close(iu)

      N = i

      if (debug_swift) then
        open(unit=iu, file='ephemeris.tmp', access='append')
        write(iu,*) '# file = ', trim(filename)
        write(iu,*) '# JD [TDB] & d [au] & l_j2000 [deg]',
     :    ' & b_j2000 [deg]'
        do i = 1, N
          tmp = l(i)
          if (l(i).lt.0.d0) tmp = tmp+2.d0*pi_
          write(iu,*) t(i),vardist(i),tmp/deg,b(i)/deg
        enddo
        write(iu,*)
        close(iu)
      endif

      return
      end

