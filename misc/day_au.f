
      real*8 function day_au(x)
      implicit none
      include 'const.inc'
      real*8 x

      day_au = x * day*c/AU
      return
      end

