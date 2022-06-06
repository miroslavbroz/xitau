c xvpl2el.f
c Convert positions and velocities to orbital elements;
c vers. wrt. the 1st body equator.
c Miroslav Broz (miroslav.broz@email.cz), Apr 17th 2022

      program xvpl2el

      include 'common.inc'
      include '../misc/const.inc'

c input parameters
      integer npl, id(NPLMAX)
      real*8 t, y(6,NPLMAX), elmts(6), m(NPLMAX), d, epoch
      real*8 l, b

c temporary variables
      integer i
      real*8 y_ofb(6,NPLMAX),m_ofb(NPLMAX),y_jac(6,NPLMAX),tmp
      real*8 hatu(3), hatv(3), hatw(3)
      real*8 u, v, w

c read chi2.in file first
      call read_chi2(npl, m, y, d, l, b, epoch)

c i.e., 1st-body equatorial frame
c towards "observer"
      hatw(1) = cos(l)*cos(b)
      hatw(2) = sin(l)*cos(b)
      hatw(3) = sin(b)

c in (x,y) plane
      hatu(1) = sin(l)
      hatu(2) = -cos(l)
      hatu(3) = 0.d0

c perpendicular, left-handed?!
      call vproduct(hatu, hatw, hatv)

      write(*,*) '# hatu = ', hatu
      write(*,*) '# hatv = ', hatv
      write(*,*) '# hatw = ', hatw

c write header
      write(*,30)
30    format("# t [JD] & id & a [AU] & e [] & i [deg] & Omega [deg]",
     :  " & omega [deg] & M [deg]     ",
     :  " & t [Besselian year] & a [arcsec] & P [yr] & P [day]",
     :  " & n [deg/yr] & varpi [deg] wrt. P.A. direction ",
     :  " & omega0 [deg] & tau [JD] & tau [Besselian year]")

c read integration output
5     continue
        do i = 1, npl
          read(*,*,end=990,err=990) t, id(i), y(1,i), y(2,i), y(3,i),
     :      y(4,i), y(5,i), y(6,i)

c transform to 1st-body equatorial frame
          call uvw2(hatu, hatv, hatw, y(1,i), y(2,i), y(3,i), u, v, w)
          y(1,i) = u
          y(2,i) = v
          y(3,i) = w
          call uvw2(hatu, hatv, hatw, y(4,i), y(5,i), y(6,i), u, v, w)
          y(4,i) = u
          y(5,i) = v
          y(6,i) = w
        enddo

c compute jacobian coordinates
        call barycenters(npl, m, y, m_ofb, y_ofb)
        call jacobian(npl, y, y_ofb, y_jac)

c        write(*,*) 'jacobian coordinates:'
c        do i = 1, npl
c          write(*,*) t, id(i), y_jac(1,i), y_jac(2,i), y_jac(3,i),
c     :      y_jac(4,i), y_jac(5,i), y_jac(6,i)
c        enddo
c        write(*,*) 'keplerian elements:'

c write elements
        do i = 2, npl
          call e1(y_jac(1,i), y_jac(4,i), elmts, m_ofb(i), 0.d0)
          call write_elmts(t, id(i), elmts, m_ofb(i), 0.d0,d)
        enddo
      goto 5

990   continue
      stop
      end

