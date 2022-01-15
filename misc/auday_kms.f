
      real*8 function auday_kms(x)
      implicit none
      include 'const.inc'
      real*8 x

      auday_kms = x * AU/day / 1.d3
      return
      end

