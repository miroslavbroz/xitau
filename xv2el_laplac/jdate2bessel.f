      real*8 function jdate2bessel(jdate)
      real*8 jdate
      real*8 year_Bessel
      parameter(year_Bessel = 365.242198781d0)

      jdate2bessel = 1900.0d0 + (jdate - 2415020.31352d0)/year_Bessel

      return
      end

