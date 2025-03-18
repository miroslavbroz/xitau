c constraint.f
c Constraint to constrain simplex parameters.
c Miroslav Broz (miroslav.broz@email.cz), Mar 18th 2025

      real*8 function constraint(x,x1,x2)
      implicit none
      real*8 x,x1,x2,y
      y = 0.0d0
      if (x.lt.x1) y = 1.0d6 - 1.0d6*(x-x1)/(x2-x1)
      if (x.gt.x2) y = 1.0d6 + 1.0d6*(x-x2)/(x2-x1)
      constraint = y
      return
      end


