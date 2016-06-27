c distance_AB_C.f
c Distance of point C from line AB, a 2-dimensional case.
c Miroslav Broz (miroslav.broz@email.cz), Jul 24th 2015

      real*8 function distance_AB_C(A, B, C, t, extra)

      implicit none
      integer n
      real*8 A(2), B(2), C(2), t
      logical extra

      real*8 eps
      parameter(eps = 1.d-16)
      real*8 d_, AB_sq, D(2), CD

      AB_sq = (B(1)-A(1))**2 + (B(2)-A(2))**2

      if (abs(AB_sq).lt.eps) then
        extra = .TRUE.
        distance_AB_C = 0.d0
        return
      endif

      t = ((C(1)-A(1))*(B(1)-A(1)) + (C(2)-A(2))*(B(2)-A(2))) / AB_sq

      if ((t.lt.0d0).or.(t.gt.1.d0)) then
        extra = .TRUE.
        distance_AB_C = 0.d0
        return
      else
        extra = .FALSE.
      endif

      D(1) = A(1) + (B(1)-A(1))*t
      D(2) = A(2) + (B(2)-A(2))*t
      CD = sqrt((D(1)-C(1))**2 + (D(2)-C(2))**2)

      distance_AB_C = CD
      return
      end


