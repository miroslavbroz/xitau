c xvpl2el.f
c Convert positions and velocities to orbital elements.
c Miroslav Broz (miroslav.broz@email.cz), Jul 30th 2015

      program xvpl2el

      include 'common.inc'
      include '../misc/const.inc'

c input parameters
      integer npl, id(NPLMAX)
      real*8 t, y(6,NPLMAX), elmts(6), m(NPLMAX), d, epoch

c temporary variables
      integer i, k
      real*8 y21(6), y43(6), yrel(6), mtot, tmp
      real*8 r, vkepl

c read chi2.in file first
      call read_chi2(npl,m,y,d,epoch)

c write header
      write(*,30)
30    format("# t [JD] & id & a [AU] & e [] & i [deg] & Omega [deg]",
     :  " & omega [deg] & M [deg]     ",
     :  " & t [Besselian year] & a [arcsec] & P [yr] & P [day]",
     :  " & n [deg/yr] & varpi [deg] wrt. P.A. direction ",
     :  " & omega0 [deg] & tau [JD] & tau [Besselian year]")

      if (npl.ne.4) then
        write(*,*) "Error: npl.ne.4!"
        stop
      endif

c read integration output
5     continue
        do i = 1, npl
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

        do k = 1, 6
          y21(k) = y(k,2) - y(k,1)
          y43(k) = y(k,4) - y(k,3)
          yrel(k) = (m(3)*y(k,3) + m(4)*y(k,4))/(m(3)+m(4))
     :      - (m(1)*y(k,1) + m(2)*y(k,2))/(m(1)+m(2))
        enddo

c        write(*,*) '# yrel coordinates:'
c        write(*,*) '# ', (yrel(k), k=1,6)
c        r = sqrt(yrel(1)**2+yrel(2)**2+yrel(3)**2)
c        write(*,*) '# r = ', r, ' au'
c        mtot = m(1)+m(2)+m(3)+m(4)
c        vkepl = sqrt(mtot*GM_S/r)
c        write(*,*) '# mtot = ', mtot, ' M_S'
c        write(*,*) '# vkepl = ', vkepl, ' au/d'

        mtot = m(1)+m(2)
        call e1(y21(1),y21(4),elmts,mtot,0.d0)
        call write_elmts(t,-2,elmts,mtot,0.d0,d)

        mtot = m(3)+m(4)
        call e1(y43(1),y43(4),elmts,mtot,0.d0)
        call write_elmts(t,-3,elmts,mtot,0.d0,d)

        mtot = m(1)+m(2)+m(3)+m(4)
        call e1(yrel(1),yrel(4),elmts,mtot,0.d0)
        call write_elmts(t,-4,elmts,mtot,0.d0,d)

      goto 5

990   continue
      stop
      end

