c read_AO.f
c Read adaptive-optics data.
c Miroslav Broz (miroslav.broz@email.cz), Aug 20th 2020

      subroutine read_AO(filename,N,t,sigma,pixel_scale,vardist,l,b,
     :  dataset,file_OBS)

      implicit none
      include '../misc/const.inc'
      include 'simplex.inc'

      character*(*) filename
      integer N
      integer dataset(AOMAX)
      real*8 t(AOMAX),sigma(AOMAX),pixel_scale(AOMAX)
      real*8 vardist(AOMAX),l(AOMAX),b(AOMAX)
      character*255 file_OBS(AOMAX)

      real*8 rho,theta
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
        write(*,*) "read_AO.f: Error opening file '",
     :     trim(filename),"'."
        stop
      endif

5     continue
        read(10,10,err=20,end=20) str
10      format(a)
        if ((str(1:1).ne.'#').and.(length(str).gt.0)) then
          i = i+1
          if (i.le.AOMAX) then
            read(str,*,err=20,end=20) t(i),sigma(i),pixel_scale(i),
     :        vardist(i),l(i),b(i),dataset(i),file_OBS(i)
          else
            write(*,*) "read_AO.f: Error number of observations .gt. ",
     :        "AOMAX = ", AOMAX
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

