
      real*8 function au_day(x)
      implicit none
      include 'const.inc'
      real*8 x

      au_day = x*AU / (day*c)
      return
      end

