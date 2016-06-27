c merit_func.f
c Merit function to constrain simplex parameters.
c Miroslav Broz (miroslav.broz@email.cz), Jul 24th 2015

      real*8 function merit_func(x,x1,x2)
      implicit none
      real*8 x,x1,x2,y
      y = ( (x - (x1+x2)/2.) * 2./(x2-x1) )**100
      merit_func = y
      return
      end


