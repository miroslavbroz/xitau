c read_CLO.f
c Read interferometric closure phase data.
c Miroslav Broz (miroslav.broz@email.cz), Apr 2nd 2016

      subroutine read_CLO(filename, N, t, u1, v1, u2, v2, lambda_eff,
     :  band_eff, t3amp, sigma_t3amp, t3phi, sigma_t3phi, dataset)

      implicit none
      include '../misc/const.inc'
      include 'simplex.inc'

      character*(*) filename
      integer N
      real*8 t(OBSMAX), u1(OBSMAX), v1(OBSMAX), u2(OBSMAX), v2(OBSMAX),
     :  lambda_eff(OBSMAX), band_eff(OBSMAX), t3amp(OBSMAX),
     :  sigma_t3amp(OBSMAX), t3phi(OBSMAX), sigma_t3phi(OBSMAX)
      integer dataset(OBSMAX)

      integer i, length, ierr
      character*512 str

      if (filename(1:1).eq.'-') then
        N = 0
        return
      endif

      open(unit=10,file=filename,status="old",form="formatted",
     :  iostat=ierr)
      if (ierr.ne.0) then
        write(*,*) "read_CLO.f: Error opening file '",
     :     trim(filename),"'."
        stop
      endif

      i = 0
5     continue
        read(10,10,err=20,end=20) str
10      format(a)
        if ((str(1:1).ne."#").and.(length(str).gt.0)) then
          i = i+1
          if (i.le.OBSMAX) then
            read(str,*,err=20,end=20) t(i), u1(i), v1(i), u2(i), v2(i),
     :        lambda_eff(i), band_eff(i), t3amp(i), sigma_t3amp(i),
     :        t3phi(i), sigma_t3phi(i), dataset(i)

              t3phi(i) = t3phi(i)*deg
              sigma_t3phi(i) = sigma_t3phi(i)*deg
          else
            write(*,*) "read_CLO.f: Error number of observations .gt. ",
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

