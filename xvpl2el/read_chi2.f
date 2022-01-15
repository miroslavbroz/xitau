c read_chi2.f
c Read chi2.in file and extract some data.
c Miroslav Broz (miroslav.broz@email.cz), Jul 30th 2015

      subroutine read_chi2(nbod, m, elmts, d_pc, epoch)

      include 'common.inc'
      include 'const.inc'
c input
c output
      integer nbod
      real*8 m(NPLMAX), elmts(6, NPLMAX), d_pc, epoch

c constants
      integer NDIMMAX
      parameter(NDIMMAX = 40)
      integer NBODMAX
      parameter(NBODMAX=4)
      integer BANDMAX
      parameter(BANDMAX=5)

c internal
      integer i, j, k, iu, ierr, nparam, nband
      real*8 x_param(NDIMMAX)
      real*8 T_eff(NBODMAX), R_star(NBODMAX), v_rot(NBODMAX)
      real*8 zero_(BANDMAX)
      real*8 gamma, T0
      real*8 msum, P, n, a
      character*255 str
      character*80 file_SKY(NBODMAX), file_RV(NBODMAX), file_TTV,
     :  file_ECL, file_VIS, file_CLO

      iu = 10
      open(unit=iu, file="chi2.in", status="old", iostat=ierr)
      if (ierr.ne.0) then
        write(*,*) 'Error opening chi2.in file.'
        stop
      endif

5     continue
        read(iu,10,end=990,err=990) str
10      format(a)
      if (str(1:1).eq.'#') goto 5

      read(str,*,end=990,err=990) nparam
      if (nparam.gt.NDIMMAX) then
        write(*,*) 'Error nparam > NDIMMAX = ', NDIMMAX
        stop
      endif

      read(iu,*,err=990,end=990) (x_param(i), i=1,nparam)

      read(iu,*,err=990,end=990) T0
      read(iu,*,err=990,end=990) nbod
      if (nbod.gt.NBODMAX) then
        write(*,*) "# Error nbod = ", nbod, ".gt. NBODMAX = ",NBODMAX
        stop
      endif

      do i = 1, nbod
        read(iu,10,err=990,end=990) file_SKY(i)
      enddo
      do i = 1, nbod
        read(iu,10,err=990,end=990) file_RV(i)
      enddo
      read(iu,10,err=990,end=990) file_TTV
      read(iu,10,err=990,end=990) file_ECL
      read(iu,10,err=990,end=990) file_VIS
      read(iu,10,err=990,end=990) file_CLO

      read(iu,*,err=990,end=990) nband
      if (nband.gt.BANDMAX) then
        write(*,*) "# Error nband = ", nband, ".gt. BANDMAX = ", BANDMAX
        stop
      endif

      close(iu)
c
c create m(), elmts() arrays for easy manipulation
c
      j = 0
      do i = 1, nbod
        j = j+1
c        m(i) = x_param(j)*GM_S
        m(i) = x_param(j)
      enddo

c      msum = m(1)
      do i = 2, nbod
c        j = j+1
c        msum = msum + m(i)
c        P = elmts(i,1)
c        n = 2.d0*pi_/P
c        a = (msum / n**2)**(1.d0/3.d0)
c        elmts(i,1) = a
        do k = 1, 6
          j = j+1
          elmts(i,k) = x_param(j)
        enddo
        do k = 3, 6
          elmts(i,k) = elmts(i,k)*deg
        enddo
      enddo

      do i = 1, nbod
        j = j+1
        T_eff(i) = x_param(j)
      enddo
      do i = 1, nbod
        j = j+1
        R_star(i) = x_param(j)
      enddo
      do i = 1, nbod
        j = j+1
        v_rot(i) = x_param(j)
      enddo

      do i = 1, nband
        j = j+1
        zero_(i) = x_param(j)
      enddo

      j = j+1
      gamma = x_param(j)
      j = j+1
      d_pc = x_param(j)

      return

990   continue
      write(*,*) 'Error reading chi2.in file.'
      stop
      end


