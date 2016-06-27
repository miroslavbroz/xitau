
      real*8 function kms_auday(x)
      implicit none
      include 'const.inc'
      real*8 x

      kms_auday = x * 1.d3/AU * day
      return
      end

