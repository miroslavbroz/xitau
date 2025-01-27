c extrap.f
c Simple linear extrapolation from two points.
c Miroslav Broz (miroslav.broz@email.cz), Jul 22nd 2015

      real*8 function extrap(x1,x2,y1,y2,x)
      implicit none
      real*8, intent(in) :: x1,x2,y1,y2,x

      extrap = y1 + (y2-y1) * (x-x1)/(x2-x1)
      return
      end

