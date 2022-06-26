c interp2.f
c Interpolation of angles (0-360 deg).
c Miroslav Broz (miroslav.broz@email.cz), Jun 24th 2022

      double precision function interp2(x1, x2, y1, y2, x)
      implicit none
      include 'const.inc'
      real*8 x1, x2, y1, y2, x
      real*8 tmp
      real*8 interp
      tmp = y1
      if ((y2-tmp).gt.180.d0*deg) then
        tmp = tmp + 360.d0*deg
      elseif ((y2-tmp).lt.-180.d0*deg) then
        tmp = tmp - 360.d0*deg
      endif
      interp2 = interp(x1, x2, tmp, y2, x)
      return
      end


