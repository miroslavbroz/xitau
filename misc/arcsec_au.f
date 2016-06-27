
      real*8 function arcsec_au(x, d)
      implicit none
      real*8 x, d
      real*8 pi
      parameter(pi=3.1415926535d0)

      arcsec_au = d * 206264.8d0 * tan(x/3600.d0/180.d0*pi)  ! there was 206.6d3 before, corrected Nov 14th 2009
      return
      end

