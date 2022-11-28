c xvpl2el.f
c Convert positions and velocities to orbital elements;
c vers. using the Laplace plane.
c Miroslav Broz (miroslav.broz@email.cz), Jul 30th 2015

      program xvpl2el

      include '../misc/const.inc'
      include '../simplex/simplex.inc'
      include '../simplex/dependent.inc'

c input parameters
      integer id(NBODMAX)
      real*8 t, y(6,NBODMAX), elmts(6), m(NBODMAX)

c temporary variables
      integer i
      real*8 y_ofb(6,NBODMAX),m_ofb(NBODMAX),y_jac(6,NBODMAX),tmp
      real*8 Lh(3),Ls(3),ctmp(3)
      real*8 a,b

c read chi2.in file first
      call read_chi2(m, y)

c write header
      write(*,30)
30    format("# t [JD] & id & a [AU] & e [] & i [deg] & Omega [deg]",
     :  " & omega [deg] & M [deg]     ",
     :  " & t [Besselian year] & a [arcsec] & P [yr] & P [day]",
     :  " & n [deg/yr] & varpi [deg] wrt. P.A. direction ",
     :  " & omega0 [deg] & tau [JD] & tau [Besselian year]")

      open(unit=10,file='angmom.tmp',status='unknown')
      write(10,*) '# t & Lx & Ly & Lz'

      open(unit=20,file='xyz.tmp',status='unknown')
      write(20,*) '# t & x & y & z'

c read integration output
5     continue
        do i = 1, nbod
          read(*,*,end=990,err=990) t, id(i), y(1,i), y(2,i), y(3,i),
     :      y(4,i), y(5,i), y(6,i)

c adjust coordinates (to get stellar-astronomy elements)
c          tmp = y(1,i)
c          y(1,i) = y(2,i)
c          y(2,i) = -tmp
c          y(3,i) = -y(3,i)
c          tmp = y(4,i)
c          y(4,i) = y(5,i)
c          y(5,i) = -tmp
c          y(6,i) = -y(6,i)
        enddo

c compute angular momentum and rotation angles
        call angmom(nbod, m, y, Lh)
c alternatively, use only 3 bodies here!
c        call angmom(3, m, y, Lh)
        call cartesian_spherical(Lh, Ls)

        write(10,*) t, Ls(1), Ls(2)/deg, Ls(3)/deg

c rotate both r, v
        a = -Ls(2)
        b = 0.5d0*pi_-Ls(3)
!        b = 0.5d0*pi_-Ls(3)+pi_  ! i.e., opposite \dot\Omega
        do i = 1, nbod
          call rotat3(y(1,i), ctmp, a)
          call rotat2(ctmp, y(1,i), b)
          call rotat3(y(4,i), ctmp, a)
          call rotat2(ctmp, y(4,i), b)
          write(20,*) t, -i, y(1,i), y(2,i), y(3,i)
        enddo

c compute jacobian coordinates
        call barycenters(nbod,m,y,m_ofb,y_ofb)
        call jacobian(nbod,y,y_ofb,y_jac)

c        write(*,*) 'jacobian coordinates:'
c        do i = 1, nbod
c          write(*,*) t, id(i), y_jac(1,i), y_jac(2,i), y_jac(3,i),
c     :      y_jac(4,i), y_jac(5,i), y_jac(6,i)
c        enddo
c        write(*,*) 'keplerian elements:'

c write elements
        do i = 2, nbod
          call e1(y_jac(1,i),y_jac(4,i),elmts,m_ofb(i),0.d0)
          call write_elmts(t,id(i),elmts,m_ofb(i),0.d0,d_pc)
        enddo
      goto 5

990   continue

      close(10)
      close(20)
      stop
      end

