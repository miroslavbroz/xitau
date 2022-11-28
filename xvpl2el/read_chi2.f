c read_chi2.f
c Read chi2.in file and extract some data.
c Miroslav Broz (miroslav.broz@email.cz), Jul 30th 2015

      subroutine read_chi2(m, y)

      include '../misc/const.inc'
      include '../simplex/simplex.inc'
      include '../simplex/dependent.inc'
c input
c output
      real*8 m(NBODMAX), y(6,NBODMAX)

c internal
      integer i, j, k, iu, ierr

      real*8 msum
      real*8 q(NBODMAX)
      real*8 elmts(NBODMAX,6)
      character*255 str

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
      read(iu,10,err=990,end=990) file_SYN
      do i = 1, nbod
        read(iu,10,err=990,end=990) file_synth(i)
      enddo
      read(iu,10,err=990,end=990) file_SED
      do i = 1, nbod
        read(iu,10,err=990,end=990) file_absol(i)
      enddo
      read(iu,10,err=990,end=990) file_AO
      read(iu,10,err=990,end=990) file_SKY2
      read(iu,10,err=990,end=990) file_SKY3
      read(iu,10,err=990,end=990) file_OCC

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
        R_star(i) = x_param(j)
      enddo
      do i = 1, nbod
        j = j+1
        P_rot(i) = x_param(j)
      enddo
      do i = 1, nbod
        j = j+1
        metal(i) = x_param(j)
      enddo
      do i = 1, nbod
        j = j+1
        Delta_t(i) = x_param(j)
      enddo
      do i = 1, nbod
        j = j+1
        C20(i) = x_param(j)
      enddo
      do i = 1, nbod
        j = j+1
        pole_l(i) = x_param(j)*deg
      enddo
      do i = 1, nbod
        j = j+1
        pole_b(i) = x_param(j)*deg
      enddo
      do i = 1, nbod
        j = j+1
        phi0(i) = x_param(j)*deg
      enddo
      do i = 1, nbod
        j = j+1
        albedo(i) = x_param(j)
      enddo

      do i = 1, 4
        j = j+1
        scattering(i) = x_param(j)
      enddo
      scattering(4) = scattering(4)*deg  ! bartheta

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

c ordering...
      do i = 1, nbod
        do j = 1, 6
          y(j,i) = elmts(i,j)
        enddo
      enddo

      return

990   continue
      write(*,*) 'Error reading chi2.in file.'
      stop
      end


