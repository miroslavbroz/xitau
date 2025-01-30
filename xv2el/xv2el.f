c xv2el.f
c Convert positions and velocities to orbital elements.
c Miroslav Broz (miroslav.broz@email.cz), Jul 30th 2015

      program xv2el

      include '../misc/const.inc'
      include '../chi2/chi2.inc'
      include '../chi2/dependent.inc'

c input parameters
      integer nbod_, id(NBODMAX)
      real*8 t, y(6,NBODMAX), elmts(6), m(NBODMAX)

c temporary variables
      integer i
      real*8 y_ofb(6,NBODMAX),m_ofb(NBODMAX),y_jac(6,NBODMAX),tmp

c read chi2.in
      call read_chi2(m, y)

c write header
      write(*,30)
30    format("# t [JD] & id & a [AU] & e [] & i [deg] & Omega [deg]",
     :  " & omega [deg] & M [deg]     ",
     :  " & t [Besselian year] & a [arcsec] & P [yr] & P [day]",
     :  " & n [deg/yr] & varpi [deg] wrt. P.A. direction ",
     :  " & omega0 [deg] & tau [JD] & tau [Besselian year]")

c read integration output
5     continue
        do i = 1, nbod
          read(*,*,end=990,err=990) t, id(i), y(1,i), y(2,i), y(3,i),
     :      y(4,i), y(5,i), y(6,i)

c adjust coordinates (to get stellar-astronomy elements)
          tmp = y(1,i)
          y(1,i) = y(2,i)
          y(2,i) = -tmp
          y(3,i) = -y(3,i)
          tmp = y(4,i)
          y(4,i) = y(5,i)
          y(5,i) = -tmp
          y(6,i) = -y(6,i)
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
      stop
      end

