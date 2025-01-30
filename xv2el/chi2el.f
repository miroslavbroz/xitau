c chi2el.f
c Read chi2.in file with masses, positions, velocities
c   and output orbital elements.
c Miroslav Broz (miroslav.broz@email.cz), Jul 29th 2015

      program chi2el

      include 'common.inc'

c input parameters
      integer nbod, nparam, id
      real*8 t, y(6, NPLMAX), elmts(6), m(NPLMAX), d, epoch

c temporary variables
      integer i, j, k
      real*8 y_ofb(6, NPLMAX), m_ofb(NPLMAX)
      real*8 y_jac(6, NPLMAX)

c read chi2.in file first
      call read_chi2(m, y)

      do i = 1, nbod
        write(*,*) '# m(i) = ', m(i)
      enddo
      do i = 1, nbod
        write(*,*) '# y(i,j) = ', (y(j,i), j = 1,6)
      enddo
      write(*,*) '# d = ', d, ' pc'

c compute barycenters in a hierarchic way
      call barycenters(nbod,m,y,m_ofb,y_ofb)
      
      do i = 1, nbod
        write(*,*) '# m_ofb(i) = ', m_ofb(i)
      enddo
      do i = 1, nbod
        write(*,*) '# y_ofb(i,j) = ', (y_ofb(j,i), j = 1,6)
      enddo

c convert coordinates to jacobian
      call jacobian(nbod,y,y_ofb,y_jac)

      do i = 1, nbod
        write(*,*) '# y_jac(i,j) = ', (y_jac(j,i), j = 1,6)
      enddo

c write header
      write(*,30)
30    format("# t [JD] & id & a [AU] & e [] & i [deg] & Omega [deg]",
     :  " & omega [deg] & M [deg]     ",
     :  " & t [Besselian year] & a [arcsec] & P [yr] & P [day]",
     :  " & n [deg/yr] & varpi [deg] wrt. P.A. direction ",
     :  " & omega0 [deg] & tau [JD] & tau [Besselian year]")

      do i = 2, nbod
        id = i-1

c calculate elements
        call e1(y_jac(1,i),y_jac(4,i),elmts,m_ofb(i),0.d0)

c write output
        call write_elmts(epoch,id,elmts,m_ofb(i),0.d0,d)

      enddo
      stop

990   continue
      write(*,*) "Error reading standard output."
      stop
      end


