
      real*8 function au_arcsec(x, d)
      implicit none
      include 'const.inc'
      real*8 x, d

      au_arcsec = atan(x*au/(d*pc))/pi_*180.d0*3600.d0
      return
      end

