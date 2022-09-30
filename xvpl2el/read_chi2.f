c read_chi2.f
c Read chi2.in file and extract some data.
c Miroslav Broz (miroslav.broz@email.cz), Jul 30th 2015

      subroutine read_chi2(nbod, m, elmts, d_pc, epoch)

      include 'common.inc'
      include '../misc/const.inc'
c input
c output
      integer nbod
      real*8 m(NPLMAX), elmts(6, NPLMAX), d_pc, epoch

c constants
      integer NDIMMAX
      parameter(NDIMMAX = 46)
      integer NBODMAX
      parameter(NBODMAX=4)
      integer BANDMAX
      parameter(BANDMAX=5)

c internal
      integer i, j, k, iu, ierr, nparam, nband
      real*8 x_param(NDIMMAX)
      real*8 T_eff(NBODMAX), logg(NBODMAX), v_rot(NBODMAX)
      real*8 metal(NBODMAX), Delta_t(NBODMAX)
      real*8 q(NBODMAX)
      real*8 zero_(BANDMAX)
      real*8 gamma, T0
      real*8 msum
      character*255 str
      character*80 file_SKY(NBODMAX), file_RV(NBODMAX), file_TTV,
     :  file_ECL, file_VIS, file_CLO
      logical debug

      debug = .true.

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
c Note: do NOT use *GM_S here!
      j = 0
c      do i = 1, nbod
c        j = j+1
c        m(i) = x_param(j)
c      enddo
      j = j+1
      msum = x_param(j)
      do i = 2, nbod
        j = j+1
        q(i) = x_param(j)
      enddo

      do i = 2, nbod
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
        logg(i) = x_param(j)
      enddo
      do i = 1, nbod
        j = j+1
        v_rot(i) = x_param(j)
      enddo
      do i = 1, nbod
        j = j+1
        metal(i) = x_param(j)
      enddo
      do i = 1, nbod
        j = j+1
        Delta_t(i) = x_param(j)
      enddo

      do i = 1, nband
        j = j+1
        zero_(i) = x_param(j)
      enddo

      j = j+1
      gamma = x_param(j)
      j = j+1
      d_pc = x_param(j)
c
c convert ratios to masses
c
      m(1) = msum
      do i = 2, nbod
        m(1) = m(1)/(1.d0+q(i))
        m(i) = 0.d0
      enddo
      do i = 2, nbod
        do j = 1, i-1
          m(i) = m(i)+m(j)
        enddo
        m(i) = q(i)*m(i)
      enddo

      if (debug) then
        do i = 1, nbod
          write(*,*) '# m(', i, ') = ', m(i), ' M_S'
        enddo
      endif

      return

990   continue
      write(*,*) 'Error reading chi2.in file.'
      stop
      end


