c jacobian.f
c Compute Jacobian coordinates if known barycenters.
c Miroslav Broz (miroslav.broz@email.cz), Jul 29th 2015

      subroutine jacobian(nbod,y,y_ofb,y_jac)

      include 'common.inc'
      integer nbod
      real*8 y(6,NPLMAX), y_ofb(6,NPLMAX), y_jac(6,NPLMAX)

      integer i,j

      do j = 1, 6
        y_jac(j,1) = 0.d0
      enddo
      do i = 2, nbod
        do j = 1, 6
          y_jac(j,i) = y(j,i) - y_ofb(j,i-1)
        enddo
      enddo

      return
      end

