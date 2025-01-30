c barycenters.f
c Compute barycenters in a hierarchic way.
c Miroslav Broz (miroslav.broz@email.cz), Jul 29th 2015

      subroutine barycenters(nbod,m,y,m_ofb,y_ofb)

      include 'common.inc'
      integer nbod
      real*8 m(NPLMAX),y(6,NPLMAX)
      real*8 m_ofb(NPLMAX),y_ofb(6,NPLMAX)

      integer i,j

      m_ofb(1) = m(1)
      do j = 1, 6
        y_ofb(j,1) = y(j,1)*m(1)
      enddo
      do i = 2, nbod
        m_ofb(i) = m_ofb(i-1) + m(i)
        do j = 1, 6
          y_ofb(j,i) = y_ofb(j,i-1) + y(j,i)*m(i)
        enddo
      enddo
      do i = 1, nbod
        do j = 1, 6
          y_ofb(j,i) = y_ofb(j,i)/m_ofb(i)
        enddo
      enddo

      return
      end

