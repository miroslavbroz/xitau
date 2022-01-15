
      real*8 function arcsec_au(x, d)
      implicit none
      include 'const.inc'
      real*8 x, d

      arcsec_au = d * pc/au * tan(x/3600.d0/180.d0*pi_)  ! there was 206.6d3 before, corrected Nov 14th 2009
      return
      end

