c interp.f
c Simple linear interpolation between two points.
c Miroslav Broz (miroslav.broz@email.cz), Jul 22nd 2015

      real*8 function interp(x1,x2,y1,y2,x)
      implicit none
      real*8 x1,x2,y1,y2,x

      if ((x.lt.x1).or.(x.gt.x2)) then
        write(*,*) "interp.f: Error: Extrapolation is not allowed!"
        write(*,*) "x1 = ", x1
        write(*,*) "x2 = ", x2
        write(*,*) "y1 = ", y1
        write(*,*) "y2 = ", y2
        write(*,*) "x = ", x
        stop
      endif

      interp = y1 + (y2-y1) * (x-x1)/(x2-x1)
      return
      end

