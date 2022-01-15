c xvpl2el.f
c Convert positions and velocities to orbital elements.
c Miroslav Broz (miroslav.broz@email.cz), Jul 30th 2015

      program xvpl2el

      include 'common.inc'
      include 'const.inc'

c input parameters
      integer npl, id(NPLMAX)
      real*8 t, y(6,NPLMAX), elmts(6), m(NPLMAX), d, epoch
      real*8 mass(NPLMAX)
      real*8 xh(NPLMAX), yh(NPLMAX), zh(NPLMAX)
      real*8 vxh(NPLMAX), vyh(NPLMAX), vzh(NPLMAX)
      real*8 xj(NPLMAX), yj(NPLMAX), zj(NPLMAX)
      real*8 vxj(NPLMAX), vyj(NPLMAX), vzj(NPLMAX)

c temporary variables
      integer i, ialpha
      real*8 gmsum,a,e,inc,capom,omega,capm,msum,tmp

c read chi2.in file first
      call read_chi2(npl,m,y,d,epoch)

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
          read(*,*,end=990,err=990) t, id(i), xh(i), yh(i), zh(i),
     :      vxh(i), vyh(i), vzh(i)
          mass(i) = m(i)*GM_S

c adjust coordinates (to get stellar-astronomy elements)
          tmp = xh(i)
          xh(i) = yh(i)
          yh(i) = -tmp
          zh(i) = -zh(i)
          tmp = vxh(i)
          vxh(i) = vyh(i)
          vyh(i) = -tmp
          vzh(i) = -vzh(i)
        enddo

c compute jacobian coordinates
        call coord_h2j(npl,mass,xh,yh,zh,vxh,vyh,vzh,xj,yj,zj,
     :    vxj,vyj,vzj)

c write elements
        gmsum = mass(1)
        msum = m(1)
        do i = 2, npl
          gmsum = gmsum + mass(i)
          call orbel_xv2el(xj(i),yj(i),zj(i),vxj(i),vyj(i),vzj(i),gmsum,
     :      ialpha,a,e,inc,capom,omega,capm)
          elmts(1) = a
          elmts(2) = e
          elmts(3) = inc
          elmts(4) = capom  ! node
          elmts(5) = omega  ! peri
          elmts(6) = capm
          msum = msum + m(i)
          call write_elmts(t,id(i),elmts,gmsum,0.d0,d)
        enddo
      goto 5

990   continue
      stop
      end

