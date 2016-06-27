c normalize.f
c Normalisation of a 3-dimensional vector.
c Miroslav Broz (miroslav.broz@email.cz), Aug 6th 2007

      subroutine normalize(x)
      real*8 x(3)
      real*8 n
      
      n = sqrt(x(1)*x(1) + x(2)*x(2) + x(3)*x(3))

      x(1) = x(1)/n
      x(2) = x(2)/n
      x(3) = x(3)/n

      return
      end


