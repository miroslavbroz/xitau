c baryc.f
c Convert heliocentric coordinates to barycentric.
c Miroslav Broz (miroslav.broz@email.cz), Nov 14th 2009

      subroutine baryc(NBOD,m,r_H,v_H,r_B,v_B)

      implicit none
      include 'simplex.inc'

      integer NBOD
      real*8 m(NBODMAX),r_H(NBODMAX,3),v_H(NBODMAX,3),r_B(NBODMAX,3),
     :  v_B(NBODMAX,3)

      integer i,j
      real*8 r_baryc(3),v_baryc(3),Sm,Srm,Svm

      Sm = 0.d0
      do i = 1,NBOD
        Sm = Sm + m(i)
      enddo

      do j = 1,3
        Srm = 0.d0
        Svm = 0.d0
        do i = 1,NBOD
          Srm = Srm + r_H(i,j)*m(i)
          Svm = Svm + v_H(i,j)*m(i)
        enddo
        r_baryc(j) = Srm/Sm
        v_baryc(j) = Svm/Sm
      enddo

      do i = 1,NBOD
        do j = 1,3
          r_B(i,j) = r_H(i,j) - r_baryc(j)
          v_B(i,j) = v_H(i,j) - v_baryc(j)
        enddo
      enddo

      return
      end


